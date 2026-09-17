#!/bin/bash
# Runs the sfizz virtual piano (Salamander Grand Piano) and keeps it wired to
# the Novation Launchkey MK4's MIDI output. Piano audio is routed through a
# "piano-volume" PipeWire loopback sink so it has its own independent volume
# control (e.g. `pactl set-sink-volume input.piano-volume 80%`) separate from
# everything else and from the interface's master level. The loopback's own
# output follows the default sink automatically, same as any regular app.

SFZ_FILE="$HOME/.local/share/sfz/SalamanderGrandPianoV3_OggVorbis/SalamanderGrandPianoV3.sfz"
CLIENT_NAME="sfizz_piano"
VOLUME_SINK="piano-volume"

if ! pgrep -f "sfizz_jack --jack_autoconnect=true --client_name=$CLIENT_NAME" >/dev/null; then
    # sfizz_jack's interactive text interface busy-loops (and eventually
    # crashes) on an immediate stdin EOF, so keep stdin open forever via a
    # pipe from `sleep infinity` instead of </dev/null.
    sleep infinity | sfizz_jack --jack_autoconnect=true --client_name="$CLIENT_NAME" "$SFZ_FILE" &
fi

if ! pgrep -f "pw-loopback -n $VOLUME_SINK " >/dev/null; then
    pw-loopback -n "$VOLUME_SINK" -c 2 -m '[FL,FR]' \
        -i '{ media.class=Audio/Sink node.description="Piano Volume" }' &
fi

while true; do
    sleep 5

    midi_port=$(pw-link -o 2>/dev/null | grep -F "Launchkey MK4 49: MIDI In (capture)")
    [ -n "$midi_port" ] && pw-link "$midi_port" "${CLIENT_NAME}:input" 2>/dev/null

    for out in output_1 output_2; do
        for dst in $(pw-link -l 2>/dev/null | awk -v c="${CLIENT_NAME}:${out}" '
            $0==c {f=1; next}
            f && /^  \|->/ {sub(/^  \|-> /,""); print; next}
            {f=0}
        '); do
            echo "$dst" | grep -qF "input.${VOLUME_SINK}:" || pw-link -d "${CLIENT_NAME}:${out}" "$dst" 2>/dev/null
        done
    done

    pw-link "${CLIENT_NAME}:output_1" "input.${VOLUME_SINK}:playback_FL" 2>/dev/null
    pw-link "${CLIENT_NAME}:output_2" "input.${VOLUME_SINK}:playback_FR" 2>/dev/null
done
