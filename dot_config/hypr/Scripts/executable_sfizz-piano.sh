#!/bin/bash
# Runs the sfizz virtual piano (Salamander Grand Piano) and keeps it wired to
# the Novation Launchkey MK4's MIDI output and whatever the current default
# audio sink is.

SFZ_FILE="$HOME/.local/share/sfz/SalamanderGrandPianoV3_OggVorbis/SalamanderGrandPianoV3.sfz"
CLIENT_NAME="sfizz_piano"

if ! pgrep -f "sfizz_jack --jack_autoconnect=true --client_name=$CLIENT_NAME" >/dev/null; then
    # sfizz_jack's interactive text interface busy-loops (and eventually
    # crashes) on an immediate stdin EOF, so keep stdin open forever via a
    # pipe from `sleep infinity` instead of </dev/null.
    sleep infinity | sfizz_jack --jack_autoconnect=true --client_name="$CLIENT_NAME" "$SFZ_FILE" &
fi

while true; do
    sleep 5

    midi_port=$(pw-link -o 2>/dev/null | grep -F "Launchkey MK4 49: MIDI In (capture)")
    [ -n "$midi_port" ] && pw-link "$midi_port" "${CLIENT_NAME}:input" 2>/dev/null

    default_sink=$(pactl get-default-sink 2>/dev/null)
    if [ -n "$default_sink" ]; then
        pw-link "${CLIENT_NAME}:output_1" "${default_sink}:playback_FL" 2>/dev/null
        pw-link "${CLIENT_NAME}:output_2" "${default_sink}:playback_FR" 2>/dev/null
    fi
done
