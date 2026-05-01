#!/usr/bin/env python3
from __future__ import annotations

import argparse
import concurrent.futures
import os
import re
import subprocess
import sys
import time
from dataclasses import dataclass
from pathlib import Path


ROOT = Path(__file__).resolve().parent.parent
TEST_PROJECT = ROOT / "tests" / "Kappa.Compiler.Tests" / "Kappa.Compiler.Tests.fsproj"
SUMMARY_RE = re.compile(r"Failed:\s*(\d+),\s*Passed:\s*(\d+),\s*Skipped:\s*(\d+),\s*Total:\s*(\d+)")


@dataclass(frozen=True)
class Shard:
    label: str
    filter_text: str
    expected_seconds: float


@dataclass
class Bucket:
    index: int
    shards: list[Shard]
    expected_seconds: float

    @property
    def label(self) -> str:
        return f"bucket-{self.index:02d}"

    @property
    def filter_text(self) -> str:
        return "|".join(f"({shard.filter_text})" for shard in self.shards)


@dataclass(frozen=True)
class RunTask:
    label: str
    filter_text: str
    expected_seconds: float
    shard_count: int


def make_shard(label: str, filter_text: str, expected_seconds: float) -> Shard:
    return Shard(label=label, filter_text=filter_text, expected_seconds=expected_seconds)


def add_family(
    shards: list[Shard],
    prefix: str,
    count: int,
    expected_seconds: float | list[float],
    filter_prefix: str | None = None,
) -> None:
    filter_base = filter_prefix or prefix

    for index in range(count):
        if isinstance(expected_seconds, list):
            expected = expected_seconds[index]
        else:
            expected = expected_seconds

        shards.append(
            make_shard(
                label=f"{prefix}{index}",
                filter_text=f"FullyQualifiedName~{filter_base}{index}",
                expected_seconds=expected,
            )
        )


def build_shards() -> list[Shard]:
    shards: list[Shard] = []

    for index in range(24):
        shards.append(
            make_shard(
                label=f"FixtureTestsShard{index}",
                filter_text=(
                    f"FullyQualifiedName~FixtureTestSupport+FixtureTestsShard{index}.raw kp fixtures satisfy their assertions shard {index}"
                ),
                expected_seconds=38.0 if index < 23 else 22.0,
            )
        )

    add_family(shards, "SmokeTestsShard", 8, 24.0, "SmokeTestSupport+SmokeTestsShard")
    add_family(shards, "ObservabilityTestsShard", 8, 26.0, "ObservabilityTestSupport+ObservabilityTestsShard")
    add_family(shards, "CoreTestsShard", 4, 23.0, "CoreTestSupport+CoreTestsShard")
    add_family(shards, "MilestoneFourTestsShard", 4, [31.0, 27.0, 38.0, 31.0], "MilestoneFourTestSupport+MilestoneFourTestsShard")
    add_family(
        shards,
        "BackendTestsShard",
        8,
        [30.0, 35.0, 31.0, 28.0, 54.0, 23.0, 25.0, 34.0],
        "BackendTestSupport+BackendTestsShard",
    )
    add_family(shards, "IlBackendTestsShard", 4, 18.0, "IlBackendTestSupport+IlBackendTestsShard")
    add_family(shards, "MilestoneThreeTestsShard", 3, [29.0, 24.0, 28.0], "MilestoneThreeTestSupport+MilestoneThreeTestsShard")
    add_family(shards, "ZigBackendTestsShard", 4, [35.0, 30.0, 26.0, 27.0], "ZigBackendTestSupport+ZigBackendTestsShard")

    shards.append(make_shard("FuzzballInspiredTests", "FullyQualifiedName~FuzzballInspiredTests", 21.0))
    shards.append(make_shard("MilestoneTwoTests", "FullyQualifiedName~MilestoneTwoTests", 29.0))
    shards.append(make_shard("ResourceModelTests", "FullyQualifiedName~ResourceModelTests", 8.0))
    shards.append(make_shard("SymbolicIdentityTests", "FullyQualifiedName~SymbolicIdentityTests", 6.0))

    return sorted(shards, key=lambda shard: shard.expected_seconds, reverse=True)


def build_buckets(shards: list[Shard], bucket_count: int) -> list[Bucket]:
    buckets = [Bucket(index=index, shards=[], expected_seconds=0.0) for index in range(bucket_count)]

    for shard in shards:
        bucket = min(buckets, key=lambda current: current.expected_seconds)
        bucket.shards.append(shard)
        bucket.expected_seconds += shard.expected_seconds

    return sorted(
        [bucket for bucket in buckets if bucket.shards],
        key=lambda bucket: bucket.expected_seconds,
        reverse=True,
    )


def kill_testhosts() -> None:
    subprocess.run(
        ["powershell", "-NoProfile", "-Command", "Get-Process testhost* -ErrorAction SilentlyContinue | Stop-Process -Force"],
        cwd=ROOT,
        check=True,
    )


def build_tests() -> None:
    subprocess.run(["dotnet", "build", str(TEST_PROJECT), "--no-restore"], cwd=ROOT, check=True)


def run_task(task: RunTask) -> tuple[RunTask, int, float, int, str]:
    command = [
        "dotnet",
        "test",
        str(TEST_PROJECT),
        "--no-build",
        "--no-restore",
        "--filter",
        task.filter_text,
    ]
    started = time.monotonic()
    completed = subprocess.run(command, cwd=ROOT, capture_output=True, text=True)
    elapsed = time.monotonic() - started
    output = completed.stdout + completed.stderr
    match = SUMMARY_RE.search(output)
    total = int(match.group(4)) if match else 0
    return task, completed.returncode, elapsed, total, output


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Run the Kappa test suite in balanced parallel buckets.")
    parser.add_argument("--build", action="store_true", help="Build the test project before running buckets.")
    parser.add_argument("--workers", type=int, default=min(12, os.cpu_count() or 12))
    parser.add_argument(
        "--buckets",
        type=int,
        default=0,
        help="Explicit bucket count. Defaults to the worker count.",
    )
    parser.add_argument(
        "--mode",
        choices=["grouped", "hybrid"],
        default="grouped",
        help="Use all-grouped buckets or dedicated fixture shards plus grouped non-fixture buckets.",
    )
    parser.add_argument(
        "--fixture-workers",
        type=int,
        default=24,
        help="Maximum concurrent fixture shard processes in hybrid mode.",
    )
    parser.add_argument(
        "--nonfixture-workers",
        type=int,
        default=8,
        help="Maximum concurrent grouped non-fixture processes in hybrid mode.",
    )
    parser.add_argument("--list", action="store_true", help="Print bucket assignments and exit.")
    return parser.parse_args()


def make_grouped_tasks(shards: list[Shard], bucket_count: int) -> list[RunTask]:
    return [
        RunTask(
            label=bucket.label,
            filter_text=bucket.filter_text,
            expected_seconds=bucket.expected_seconds,
            shard_count=len(bucket.shards),
        )
        for bucket in build_buckets(shards, min(bucket_count, len(shards)))
    ]


def make_hybrid_tasks(args: argparse.Namespace, shards: list[Shard]) -> tuple[list[RunTask], list[RunTask]]:
    fixture_shards = [shard for shard in shards if shard.label.startswith("FixtureTestsShard")]
    nonfixture_shards = [shard for shard in shards if not shard.label.startswith("FixtureTestsShard")]

    fixture_tasks = [
        RunTask(
            label=shard.label,
            filter_text=shard.filter_text,
            expected_seconds=shard.expected_seconds,
            shard_count=1,
        )
        for shard in fixture_shards
    ]

    nonfixture_bucket_count = args.buckets if args.buckets > 0 else args.nonfixture_workers
    nonfixture_tasks = make_grouped_tasks(nonfixture_shards, nonfixture_bucket_count)
    return fixture_tasks, nonfixture_tasks


def print_task_plan(label: str, tasks: list[RunTask]) -> None:
    print(f"{label}: {len(tasks)} tasks", flush=True)
    for task in tasks:
        print(f"  - {task.label} ({task.expected_seconds:.1f}s, {task.shard_count} shards)", flush=True)


def execute_tasks(tasks: list[RunTask], workers: int) -> tuple[int, list[tuple[str, str]], float]:
    total_tests = 0
    failures: list[tuple[str, str]] = []
    started = time.monotonic()

    with concurrent.futures.ThreadPoolExecutor(max_workers=min(workers, len(tasks))) as pool:
        future_map = {pool.submit(run_task, task): task for task in tasks}

        for future in concurrent.futures.as_completed(future_map):
            task, return_code, elapsed, count, output = future.result()
            total_tests += count

            if return_code == 0:
                print(
                    f"[PASS] {task.label} ({elapsed:.1f}s, {count} tests, {task.shard_count} shards)",
                    flush=True,
                )
            else:
                print(f"[FAIL] {task.label} ({elapsed:.1f}s)", flush=True)
                failures.append((task.label, output))

    return total_tests, failures, time.monotonic() - started


def main() -> int:
    args = parse_args()

    if args.build:
        kill_testhosts()
        build_tests()

    shards = build_shards()
    total_expected = sum(shard.expected_seconds for shard in shards)

    if args.mode == "grouped":
        bucket_count = args.buckets if args.buckets > 0 else args.workers
        tasks = make_grouped_tasks(shards, bucket_count)

        if args.list:
            print_task_plan("grouped", tasks)
            return 0

        critical_path = max(task.expected_seconds for task in tasks)
        print(
            f"Running {len(shards)} shards in {len(tasks)} grouped tasks with {args.workers} workers.",
            flush=True,
        )
        print(
            f"Expected serial time {total_expected:.1f}s, expected critical path {critical_path:.1f}s, balance factor {total_expected / critical_path:.2f}.",
            flush=True,
        )
        total_tests, failures, wall_time = execute_tasks(tasks, args.workers)
    else:
        fixture_tasks, nonfixture_tasks = make_hybrid_tasks(args, shards)

        if args.list:
            print_task_plan("fixture", fixture_tasks)
            print_task_plan("nonfixture", nonfixture_tasks)
            return 0

        fixture_expected = max(task.expected_seconds for task in fixture_tasks)
        nonfixture_expected = max(task.expected_seconds for task in nonfixture_tasks)
        print(
            f"Running {len(fixture_tasks)} fixture tasks with {args.fixture_workers} workers and {len(nonfixture_tasks)} grouped non-fixture tasks with {args.nonfixture_workers} workers.",
            flush=True,
        )
        print(
            f"Expected serial time {total_expected:.1f}s, expected fixture path {fixture_expected:.1f}s, expected non-fixture path {nonfixture_expected:.1f}s.",
            flush=True,
        )

        started = time.monotonic()
        total_tests = 0
        failures: list[tuple[str, str]] = []

        with concurrent.futures.ThreadPoolExecutor(max_workers=2) as supervisor:
            fixture_future = supervisor.submit(execute_tasks, fixture_tasks, args.fixture_workers)
            nonfixture_future = supervisor.submit(execute_tasks, nonfixture_tasks, args.nonfixture_workers)

            for future in concurrent.futures.as_completed([fixture_future, nonfixture_future]):
                task_total_tests, task_failures, _ = future.result()
                total_tests += task_total_tests
                failures.extend(task_failures)

        wall_time = time.monotonic() - started

    print(f"Total tests: {total_tests}", flush=True)
    print(f"Wall time: {wall_time:.1f}s", flush=True)

    if failures:
        for label, output in failures:
            print(f"\n=== {label} ===\n{output}", file=sys.stderr)
        return 1

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
