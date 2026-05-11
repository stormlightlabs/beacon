from __future__ import annotations

from .models import Job, JobStatus


def describe_status(status: JobStatus) -> str:
    match status:
        case JobStatus.QUEUED:
            return "queued"
        case JobStatus.RUNNING:
            return "running"
        case JobStatus.DONE:
            return "done"


def describe_payload(job: Job[str] | None) -> str:
    if job is None:
        return "missing"

    match job.payload:
        case "":
            return "empty"
        case value if value.startswith("x"):
            return "special"
        case value:
            return value
