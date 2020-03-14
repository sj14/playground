# MacOS installer for static ffmpeg binary

## Why?

`brew install ffmpeg` installs too many dependencies.  
This script downloads the static binary and moves it to `/usr/local/bin/`, thats all.

## Usage

Latest stable version:

```bash
./install.sh
```

Latest snapshot:

```bash
./install.sh snapshot
```

### Without cloning the repository first

stable: 
```bash
curl -s https://raw.githubusercontent.com/sj14/playground/master/ffmpeg-mac-installer/install.sh | bash -s
```

snapshot:
```bash
curl -s https://raw.githubusercontent.com/sj14/playground/master/ffmpeg-mac-installer/install.sh | bash -s snapshot
```

## Updating

For updating to the latest stable or snapshot build, just run the desired command again, it will overwrite the previously installed version.