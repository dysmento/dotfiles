#!/bin/bash
# GUI password prompt for sudo, used via the SUDO_ASKPASS env var.
zenity --password --title="Authentication Required" 2>/dev/null
