"""
A collection of validation functions. These functions are used to validate function arguments.
based on code by Mattias Dittbender used in ICU-Cockpit project.
"""
import re
from typing import Any, Tuple


class ValidationError(BaseException):

    def __init__(self, message: str):
        self.message = message
        super().__init__(message)


def require_not_none(to_be_validated, failure_message: str = 'Must be specified but was None') -> None:
    """
    Check if a provided value `to_be_validated` is not None, otherwise raise exception `ValidationError` with
    `failure_message`.
    """

    if to_be_validated is None:
        raise ValidationError(message=failure_message)


def require_not_empty(to_be_validated, failure_message: str = 'Must not be empty') -> None:
    """
    Check if a provided value `to_be_validated` is not empty, otherwise raise exception `ValidationError` with
    `failure_message`.
    """

    if len(to_be_validated) == 0:
        raise ValidationError(message=failure_message)


def require_max_size(max_size: int, to_be_validated: str) -> None:
    """
    Check if a provided string `to_be_validated` does not contain more than `max_size` characters, otherwise raise
    exception `ValidationError`.
    """
    if max_size < 0:
        raise ValueError("Max size must not be negative but was <{}>".format(max_size))

    if len(to_be_validated) > max_size:
        raise ValidationError(
            message="Input must not exceed {} characters, but was {} long".format(max_size, len(to_be_validated))
        )


def require_true(condition: bool, failure_message: str = "Expected True, but was False") -> None:
    """
    Check if a provided `condition` is True, otherwise raise exception `ValidationError` with `failure_message`.
    """
    if not condition:
        raise ValidationError(message=failure_message)


def require_matches(regex, to_be_validated: str, failure_message: str) -> None:
    """
    Check if a provided string `to_be_validated` fulfills the `regex` pattern, otherwise raise exception
    `ValidationError` with `failure_message`.
    """
    if not re.match(regex, to_be_validated):
        raise ValidationError(message=failure_message)


def require_any_of(value: str, allowed_values: [str]) -> None:
    """
    Check if a provided string `value`, is contained in a string list of `allowed_values`, otherwise raise
    exception `ValidationError`.
    """
    if value not in allowed_values:
        raise ValidationError(message="{} is not an allowed value.".format(value))


def require_within_range(to_be_validated: int, lower: int, upper: int, include_lower=True, include_upper=True):
    """
    Check if a provided integer `to_be_validated` is within a specified range, otherwise raise exception `ValueError`.
    Args:
        to_be_validated: Integer to be validated
        lower: Lower bound of valid range
        upper: Upper bound of valid range
        include_lower: Include lower value if True: `lower` <= `to_be_validated`, otherwise `lower` < `to_be_validated`
        include_upper: Include upper value if True: `to_be_validated` <= upper, otherwise `to_be_validated` < `upper`
    Raises:
        ValueError: If `to_be_validated` not within specified range
    """

    if include_lower:
        above_lower_bound = lower <= to_be_validated
    else:
        above_lower_bound = lower < to_be_validated

    if include_upper:
        below_upper_bound = to_be_validated <= upper
    else:
        below_upper_bound = to_be_validated < upper

    is_in_range = above_lower_bound and below_upper_bound

    if not is_in_range:
        lower_interval_bound = "["
        upper_interval_bound = "]"
        if not include_lower:
            lower_interval_bound = "("
        if not include_upper:
            upper_interval_bound = ")"
        raise ValueError(f"Value {to_be_validated} must be within range "
                         f"{lower_interval_bound}{lower}, {upper}{upper_interval_bound}")


def require_valid_type(value: Any, allowed_types: Tuple) -> None:
    if not isinstance(value, allowed_types):
        failure_message = f"Value '{value}', must be of type '{allowed_types}', but was '{type(value)}'"
        raise ValidationError(message=failure_message)


def require_less_or_equal(left_value: Any, right_value: Any):
    if not left_value <= right_value:
        failure_message = f"Required left value <= right value, but was '{left_value}' <= '{right_value}'"
        raise ValidationError(message=failure_message)