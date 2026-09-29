#!/usr/bin/env python3
"""Crop the final Italy and Croatia map pairs to common country-specific bounds."""

from pathlib import Path

from PIL import Image, ImageChops


PROJECT_ROOT = Path(__file__).resolve().parents[2]
OUTPUT_DIR = PROJECT_ROOT / "03_output" / "04_figures"
PAIRS = {
    "Italy": (
        OUTPUT_DIR / "01_italy_share_regions.png",
        OUTPUT_DIR / "02_italy_eobs_intersections.png",
    ),
    "Croatia": (
        OUTPUT_DIR / "03_croatia_share_regions.png",
        OUTPUT_DIR / "04_croatia_eobs_intersections.png",
    ),
}


PADDING = 36
WHITE_TOLERANCE = 8


def content_bbox(image: Image.Image) -> tuple[int, int, int, int]:
    """Return the bounding box of pixels visibly different from white."""
    rgb = image.convert("RGB")
    white = Image.new("RGB", rgb.size, "white")
    difference = ImageChops.difference(rgb, white).convert("L")
    mask = difference.point(lambda value: 255 if value > WHITE_TOLERANCE else 0)
    bbox = mask.getbbox()
    if bbox is None:
        raise ValueError("Image contains no visible non-white content.")
    return bbox


def padded_union(
    boxes: list[tuple[int, int, int, int]], size: tuple[int, int]
) -> tuple[int, int, int, int]:
    left = max(0, min(box[0] for box in boxes) - PADDING)
    top = max(0, min(box[1] for box in boxes) - PADDING)
    right = min(size[0], max(box[2] for box in boxes) + PADDING)
    bottom = min(size[1], max(box[3] for box in boxes) + PADDING)
    return left, top, right, bottom


def crop_pair(country: str, paths: tuple[Path, Path]) -> None:
    images = [Image.open(path) for path in paths]
    try:
        if images[0].size != images[1].size:
            raise ValueError(f"{country}: source canvases do not have identical dimensions.")
        crop_box = padded_union([content_bbox(image) for image in images], images[0].size)
        for path, image in zip(paths, images):
            cropped = image.crop(crop_box)
            cropped.save(path, format="PNG", optimize=True)
        width = crop_box[2] - crop_box[0]
        height = crop_box[3] - crop_box[1]
        print(f"{country}: crop box {crop_box}; output size {width} x {height} px")
    finally:
        for image in images:
            image.close()


if __name__ == "__main__":
    for country_name, country_paths in PAIRS.items():
        crop_pair(country_name, country_paths)