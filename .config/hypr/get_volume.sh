#!/bin/bash

# Get current volume using wpctl
volume=$(wpctl get-volume @DEFAULT_AUDIO_SINK@)

# Extract just the volume value (remove any [MUTED] tag if present)
volume_value=$(echo "$volume" | awk '{print $2}')

echo "Current volume: $volume_value"