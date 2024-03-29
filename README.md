
A noob trying out Tk GUIs in Common Lisp.

Nothing fancy, private tests, hopefully helping someone to getting
started, until they make it to a proper tutorial.

**NEW!** Peter Lane assembled an excellent resource for Ltk and Nodgui:

- https://peterlane.codeberg.page/ltk-examples/

**NEW!** nodgui supports custom Tk themes!! See gallery below.

To test:

    git clone https://github.com/TkinterEP/ttkthemes/

at this project's root (or use the "yaru" theme, commited).


# Quick Insights

How to create widgets: `(make-instance 'widget-name)`, for example

~~~lisp
(make-instance 'frame)
(make-instance 'treeview)
~~~

How to place them on the grid:

~~~lisp
;; (grid <widget> <column> <row> &options)
(grid c 0 0 :sticky "n" ;; north
            :padx 5 :pady 5)
~~~

The rest is discoverable !


![](feet-to-meters.png)

With nodgui, yaru and breeze theme (more below):

![](feet-yaru.png)

![](feet-breeze.png)

https://gist.github.com/jasom/4c4bf02e60d85f2644f99ce7be5dce17

## Tree widget


![](a-tree.png)


~~~lisp
(make-instance 'treeview)
;; and
(treeview-insert c.tree :text "some text"))
~~~

With columns:

![](tree-columns.png)

(commented sources)

A simple example:

~~~lisp
;; Always use with-nodgui or with-ltk.
(with-nodgui ()
    (let ((tree  (make-instance 'scrolled-treeview
                                ;; a treeview always has a first column.
                                :columns (list "col2"))))
      ;; We place our widget on the grid to see it.
      (grid tree 1 0)
      (loop for data in '("aaa" "bbb" "ccc")
         do (treeview-insert-item tree
                                  ;; text of the first column.
                                  :text data
                                  ;; text of the other columns.
                                  :column-values (list "val2")))))
~~~

For collapsable rows, the use of `parents` and `children`, an example: https://notabug.org/cage/nodgui/src/7e6da313d99d4b260aadab595fe4b0f843520da7/src/demo-tests.lisp

Another example showing a treeview, with columns, inserting content
from a searchbox, clearing the tree's content, and others (resizing,
etc): see `example-treeview-search-display`.


## Media player

Using Peter Lane's listbox example: https://peterlane.codeberg.page/ltk-examples/#_more_widgets

We display a list of strings (file names), we add a select box (select
media player), a button (listen), and we play the file with an
external program.

Might become a general utility in my growing scripts: send a list of something, decide of the action, do it.

![](mediaplayer.png)

## Media player (nodgui, themes)

See musicplayer-nodgui.lisp.

We can use Tk themes, that's huge!


![](media-equilux.png)
![](media-adapta.png)
![](media-yaru.png)
![](media-arc.png)
![](media-aquativo.png)
![](media-black.png)
![](media-breeze.png)
![](media-blue.png)
![](media-clearlooks.png)
![](media-radiance.png)
![](media-plastik.png)
![](media-ubuntu.png)

I cloned [ttkthemes](https://github.com/TkinterEP/ttkthemes/) ([gallery](https://ttkthemes.readthedocs.io/en/latest/themes.html#elegance)) locally.

We need to eval a tcl file, and set the theme:

~~~lisp
;; inside nodgui:with-nodgui main loop.
(eval-tcl-file "ttkthemes/ttkthemes/png/yaru/yaru.tcl")
(use-theme "yaru")
~~~

and that's it.

The [awthemes](https://sourceforge.net/projects/tcl-awthemes/) are supposed to be supported (didn't try yet).

List of Tcl/Tk themes: https://wiki.tcl-lang.org/page/List+of+ttk+Themes

The gif themes of ttkthemes are not yet supported, but will be when `tklib` lands in Debian. Read [nodgui #13](https://wiki.tcl-lang.org/page/List+of+ttk+Themes).

note: the "scid" and "smog" themes bugged.

### More themes

[Forest theme](https://github.com/rdbende/Forest-ttk-theme)

![](media-forest-light.png)

Screenshot from its readme:

![](https://raw.githubusercontent.com/rdbende/Forest-ttk-theme/master/Forest-light%20screenshot.png)

(by the same author, [Azure](https://github.com/rdbende/Azure-ttk-theme) and [Sun Valley](https://github.com/rdbende/Sun-Valley-ttk-theme) themes didn't load)

[Ale themes](https://github.com/aplsimple/ale_themes)

![](media-lightbrown.png)



## Interactively building the GUI

http://www.peter-herth.de/ltk/ltkdoc/node8.html

run `(start-wish)` and start building interactively !

    (defparameter *button* (make-instance 'button :text "OK"))
    (grid *button* 1 0 :sticky "e")

and voilà, you didn't have to restart the main loop to see the new
button into the interface.

We didn't have to use the `with-ltk` macro too.

Then `(exit-wish)`.


## Demo

The nodgui demo shows a lot of widgets:

![](nodgui-demo-style-clam.png)

Try it out with

    (ql:quickload :nodgui)
    (nodgui.demo:demo)


# Links

- https://peterlane.codeberg.page/ltk-examples/ NEW(er) and awesome.
- https://github.com/LispCookbook/cl-cookbook/issues/31
- https://github.com/CodyReichert/awesome-cl#gui
- http://www.peter-herth.de/ltk/
- https://notabug.org/cage/nodgui.git = based on ltk with syntax sugar and more widgets (calendar, completion,…).

Tk tutorials used:

- https://tkdocs.com/tutorial/index.html
- https://www.effbot.org/tkinterbook/grid.htm

## Example apps

- https://github.com/lisp-mirror/fulci (by the author of nodgui)

![](https://www.autistici.org/interzona/img/fulci/search-frame.png)
