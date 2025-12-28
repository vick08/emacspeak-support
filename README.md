# Emacspeak Support Extensions

A collection of speech-enabled support extensions for modern Emacs packages that work with Emacspeak.

Emacspeak is an audio desktop that provides complete eyes-free access to Emacs. This repository provides speech support for modern packages that are not yet in the Emacspeak mainline, making them more accessible and easier to use with speech feedback.

## Table of Contents

- [Installation](#installation)
- [Usage](#usage)
- [Available Extensions](#available-extensions)
- [Contributing](#contributing)
- [Development Guide](#development-guide)
- [License](#license)

## Installation

### Basic Installation

1. Clone this repository:

```bash
git clone https://github.com/robertmeta/emacspeak-support.git
```

2. Add to your Emacs configuration:

```elisp
(add-to-list 'load-path "/path/to/emacspeak-support")
(require 'emacspeak-support)
```

### Auto-loading Extensions

To automatically enable all extensions when Emacspeak loads:

```elisp
(with-eval-after-load 'emacspeak-support
  (emacspeak-support-enable-all))
```

Or selectively enable extensions when their corresponding packages load:

```elisp
(with-eval-after-load 'corfu
  (emacspeak-support-enable-corfu))

(with-eval-after-load 'which-key
  (emacspeak-support-enable-which-key))

(with-eval-after-load 'markdown-mode
  (emacspeak-support-enable-markdown))
```

See `example-config.el` for a complete configuration example.

## Usage

### Enable/Disable Individual Extensions

Each extension can be toggled on or off independently:

```elisp
;; Enable specific extensions
M-x emacspeak-support-enable-corfu
M-x emacspeak-support-enable-which-key
M-x emacspeak-support-enable-markdown

;; Disable specific extensions
M-x emacspeak-support-disable-corfu
M-x emacspeak-support-disable-which-key
M-x emacspeak-support-disable-markdown

;; Toggle extensions on/off
M-x emacspeak-support-toggle-corfu
M-x emacspeak-support-toggle-which-key
M-x emacspeak-support-toggle-markdown
```

### Enable/Disable All Extensions

```elisp
;; Enable all extensions at once
M-x emacspeak-support-enable-all

;; Disable all extensions
M-x emacspeak-support-disable-all
```

### Check Extension Status

```elisp
;; Display which extensions are currently enabled
M-x emacspeak-support-status
```

### Generic Interface

You can also use the generic commands with completion:

```elisp
M-x emacspeak-support-enable    ;; prompts for extension name
M-x emacspeak-support-disable   ;; prompts for extension name
M-x emacspeak-support-toggle    ;; prompts for extension name
```

## Available Extensions

### emacspeak-corfu

Speech-enables Corfu, a modern in-buffer completion interface.

**Features:**

- Speaks completion candidates with position info (e.g., "3 of 10: candidate-name")
- Includes annotations when available
- Provides auditory icons for navigation and selection
- Real-time feedback during candidate filtering

### emacspeak-which-key

Speech-enables Which-Key, which displays available keybindings after prefix keys.

**Features:**

- Automatically speaks keybinding options when popup appears
- Page navigation with auditory feedback
- Toggle auto-speak with `emacspeak-which-key-toggle-auto-speak`
- Manual speak command: `emacspeak-which-key-speak`

### emacspeak-markdown

Speech-enables markdown-mode for better navigation of markdown documents.

**Features:**

- Speaks "heading level 3: Title" instead of "three pounds space Title"
- Smart navigation between headers, links, and list items
- Appropriate voice personalities for different markdown elements
- Keybinding: `C-c C-s h` to speak current heading

### voiceover-support

Provides VoiceOver-style navigation keybindings for Emacspeak.

**Features:**

- Familiar keybindings for users coming from macOS VoiceOver
- Enable with `emacspeak-support-enable-voiceover`

## Contributing

Contributions are welcome! This repository serves as a collection of speech support for modern Emacs packages. Whether you're adding support for a new package or improving existing extensions, your contributions help make Emacs more accessible.

### Why Contribute Here?

- **Faster iteration**: No need to go through the Emacspeak mainline review process
- **Easy management**: Each extension can be enabled/disabled independently
- **Community-driven**: Share your speech enhancements with other Emacspeak users
- **Upstream path**: Well-tested extensions can eventually be contributed to Emacspeak mainline

### What to Contribute

Good candidates for new extensions:

- Modern Emacs packages not yet supported by Emacspeak
- Packages with rich UI elements that benefit from speech feedback
- Completion frameworks, navigation tools, or information displays
- Any package you use regularly with Emacspeak that needs better speech support

### Contribution Workflow

1. **Fork the repository** and create a feature branch
2. **Create your extension** following the development guide below
3. **Test thoroughly** with Emacs 31+ and Emacspeak
4. **Submit a pull request** with:
   - Clear description of the package being speech-enabled
   - Key features your extension provides
   - Any special configuration or usage notes

### Code Standards

- Follow GNU Emacs Lisp conventions
- Include clear commentary sections
- Add enable/disable functions for toggle support
- Test all interactive commands provide appropriate feedback
- Ensure auditory icons play at appropriate times

## Development Guide

### Speech-enabling Pattern

All extensions in this repository follow a standard pattern for speech-enabling Emacs packages:

#### 1. File Structure

Create a new file named `emacspeak-PACKAGE.el` with this structure:

```elisp
;;; emacspeak-PACKAGE.el --- Speech-enable PACKAGE -*- lexical-binding: t; -*-

;; Copyright (C) YEAR YOUR NAME

;; Keywords: Emacspeak, Audio Desktop, PACKAGE

;;; Commentary:
;; Speech-enables PACKAGE, which provides...
;; Describe what the package does and what speech features you're adding

;;; Code:

(require 'emacspeak-preamble)
(require 'PACKAGE)

;; Your extension code here

(provide 'emacspeak-PACKAGE)
;;; emacspeak-PACKAGE.el ends here
```

#### 2. Core Components

**Voice Mappings**: Map package faces to Emacspeak voice personalities:

```elisp
(voice-setup-add-map
 '((package-face-name voice-personality)))
```

Common voice personalities:

- `voice-annotate` - for metadata and annotations
- `voice-bolden` - for emphasized or important text
- `voice-smoothen` - for de-emphasized text
- `voice-monotone` - for technical/literal content

**Helper Functions**: Create functions to extract and format state:

```elisp
(defun emacspeak-package--get-current-item ()
  "Extract current item for speech output."
  ;; Your logic here
  )
```

**Interactive Command Advice**: Add `after` advice to user-facing commands:

```elisp
(defadvice package-command (after emacspeak pre act comp)
  "Speak feedback after PACKAGE-COMMAND."
  (when (ems-interactive-p)
    (let ((feedback (emacspeak-package--get-current-item)))
      (dtk-speak feedback)
      (emacspeak-icon 'select-object))))
```

**Internal Function Advice**: Hook into package internals for real-time updates:

```elisp
(cl-declaim (optimize (safety 0) (speed 3)))
(defadvice package--internal-update (after emacspeak pre act comp)
  "Provide real-time speech feedback during UI updates."
  (when (ems-interactive-p)
    (dtk-speak (emacspeak-package--get-current-item))))
```

#### 3. Enable/Disable Support

Every extension should provide enable and disable functions:

```elisp
(defun emacspeak-package-enable ()
  "Enable speech support for PACKAGE."
  (interactive)
  (ad-activate 'package-command)
  (ad-activate 'package--internal-update)
  (message "Enabled Emacspeak support for PACKAGE"))

(defun emacspeak-package-disable ()
  "Disable speech support for PACKAGE."
  (interactive)
  (ad-deactivate 'package-command)
  (ad-deactivate 'package--internal-update)
  (message "Disabled Emacspeak support for PACKAGE"))
```

#### 4. Register with Main Loader

Add your extension to `emacspeak-support.el`:

```elisp
(defvar emacspeak-support--extensions
  '((corfu . "emacspeak-corfu")
    (which-key . "emacspeak-which-key")
    (markdown . "emacspeak-markdown")
    (your-package . "emacspeak-your-package"))  ;; Add this line
  "Alist of extension symbols to file names.")
```

And add convenience functions:

```elisp
(defun emacspeak-support-enable-your-package ()
  "Enable Emacspeak support for YOUR-PACKAGE."
  (interactive)
  (emacspeak-support-enable 'your-package))

(defun emacspeak-support-disable-your-package ()
  "Disable Emacspeak support for YOUR-PACKAGE."
  (interactive)
  (emacspeak-support-disable 'your-package))

(defun emacspeak-support-toggle-your-package ()
  "Toggle Emacspeak support for YOUR-PACKAGE."
  (interactive)
  (emacspeak-support-toggle 'your-package))
```

### Key Emacspeak Functions

**Speech Output:**

- `dtk-speak TEXT` - Speak text via text-to-speech
- `dtk-speak-and-echo TEXT` - Speak and display text

**Auditory Icons:**

- `(emacspeak-icon 'select-object)` - Object selection sound
- `(emacspeak-icon 'on)` - Feature enabled sound
- `(emacspeak-icon 'off)` - Feature disabled sound
- `(emacspeak-icon 'open-object)` - Opening/expanding sound
- `(emacspeak-icon 'close-object)` - Closing/collapsing sound

**Checking Context:**

- `(ems-interactive-p)` - Check if command was called interactively (use in advice)

### Testing Your Extension

1. **Load in a running Emacspeak session:**

```elisp
(load-file "/path/to/emacspeak-your-package.el")
(emacspeak-your-package-enable)
```

Or use `M-x eval-buffer` after opening the file.

2. **Verify speech feedback:**

- Interactive commands provide appropriate feedback
- UI updates speak current selection/state
- Auditory icons play at appropriate times
- Annotations are properly voiced when available

3. **Test toggle functionality:**

```elisp
M-x emacspeak-support-enable-your-package
M-x emacspeak-support-disable-your-package
M-x emacspeak-support-toggle-your-package
```

### Examples

Look at existing extensions for reference:

- `emacspeak-corfu.el` - Complex UI with annotations and position tracking
- `emacspeak-which-key.el` - Auto-speak with timer integration
- `emacspeak-markdown.el` - Custom navigation and voice personalities

### Getting Help

- Check existing extensions in this repository for patterns
- Review Emacspeak documentation at https://tvraman.github.io/emacspeak/
- Open an issue for questions or guidance

## Requirements

- Emacs 31+
- Emacspeak
- Target packages (corfu, which-key, markdown-mode, etc.) as needed

## Repository Structure

```
emacspeak-support/
├── .gitignore                 # Ignore patterns
├── README.md                  # This file
├── example-config.el          # Example configuration
├── emacspeak-support.el       # Main loader with toggle functionality
├── emacspeak-corfu.el         # Corfu support
├── emacspeak-which-key.el     # Which-Key support
├── emacspeak-markdown.el      # Markdown-mode support
└── voiceover-support.el       # VoiceOver-style keybindings
```

## License

GNU General Public License v2.0 or later

See the individual files for copyright and license information.
