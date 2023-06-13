
2023-05-12: Set up for SBCL, abandon doom emacs, but learn trheir set up 

Project structure inspired by :

- https://github.com/ChristianChiarulli/lvim
- https://github.com/doomemacs/doomemacs

## Set up 

### Ivy 
https://oremacs.com/swiper/#installation
```
cd plugins 
git clone https://github.com/abo-abo/swiper
cd swiper && make compile
```

### To clone

```
git clone git remote add origin https://github.com/av-maslov/emacs-minimal2.git ~/.emacs.d
git clone https://github.com/av-maslov/emacs-yasnippets.git ~/.emacs.d/plugins-yasnippet-snippets
```

### Install ESS and Julia mode for it (CentOS)

```
wget https://ess.r-project.org/downloads/ess/ess-18.10.2.tgz
mkdir ESS
tar xzvf ess-18.10.2.tgz -C ESS/
rm ess-18.10.2.tgz
rm -r ESS/ess-18.10.2
```

or

```
git clone https://github.com/emacs-ess/ESS.git ~/ESS
git clone https://github.com/JuliaEditorSupport/julia-emacs.git ~/.emacs.d/plugins/
```

add 

```
(add-to-list 'load-path "~/ESS/lisp")
(load "~/ESS/lisp/ess-site")
```

**Errors**

- Mac: `error: Invalid face, linum`, caused by `colors.el`: `..linum, darkgrey theme?`

**For ESS-mode**

For brackets auto-closed ```M-x electric-pair-mode``` NOT `M-x paredit-mode` which adds extra space

**Install Fira Code font**

- https://github.com/tonsky/FiraCode

Windows: 

```
choco install firacode
```

**For YA Snippet use** 
```
Shift TAB
```
see init.el 

# Some custom key-bindings

```
C-c S : easy motion 
C-l   : go to line
C RET : New line without breaking the current line
C TAB : other window 
```

# Packages requiring installation from MELPA

```
M-x package-list-pcakages
```
or
```
M-x package-install
```

- [auto-complete/auto-complete](https://github.com/auto-complete/auto-complete) , http://auto-complete.org
- [magit-status](http://magit.vc/)
- [slime](https://common-lisp.net/project/slime/)
- [symon](https://melpa.org/?utm_source=dlvr.it&utm_medium=twitter#/symon) tiny graphical system monitor


# Packages requiring compilation

```
M-x byte-compile rainbow-delimiters.el 
```


# Emacs minimal settings

Preinstalled packages:  

- [Solarized color theme](https://github.com/sellout/emacs-color-theme-solarized)
- [smex](https://github.com/nonsequitur/smex/)
- [ace-jump-mode](https://github.com/winterTTr/ace-jump-mode)
- [highlight-symbol.el](https://github.com/nschum/highlight-symbol.el)
- [SrSpeedbar](http://www.emacswiki.org/emacs/SrSpeedbar)
- [scala-mode2](https://github.com/hvesalai/scala-mode2)


