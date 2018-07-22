# Hugo posts with Emacs

Using Emacs, I create Hugo posts with these macros and stuff

### Prerequisites

[Emacs](https://www.gnu.org/software/emacs/download.html), [Hugo](https://gohugo.io/)

### Installing

Clone this repo somewhere that Emacs can see

```
git clone git clone git@github.com:thunderrabbit/emacs-hugo-posts.git hugo
```

Tell `.emacs` to load the macros

```
(add-to-list 'load-path "~/path/to/hugo/")
(load "hugo.el")
```

## License

This project is licensed under the GNU General Public License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Fred Nugen
* Christian C. Salvadó
* https://www.reddit.com/user/wasamasa
* Pascal J Bourguignon
