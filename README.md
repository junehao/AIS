===
# AIS: Architectural Information System

- Auhtor: June-Hao Hou <junehao@gmail.com>
- Version: 0.8beta
- Initial Release: April 8, 2006
- Updated: December 4, 2023

## ABOUT

AIS is an experimental associative modeling library that implementes an object-oriented (or more specifically, abstraction-oriented) modeling environment for constructing stair-like abstract structure, namely "stairness". This system is designed by June-Hao Hou as partial fulfillment of his DDes thesis.

## INSTALLATION

Place the AIS folder anywhere in your hard driver, then follow the steps to install:

1. Launch AutoCAD.
2. Select `Tools > Options...` to open the Options panel.
3. In Files tab, find Support File Search Path (should be the first item) and expand it.
4. Click "Add..." then "Browse..." to open the Browse for Folder dialog box.
5. Browse to the AIS\bin folder, click "OK".

That's it! Next time when you launch AutoCAD, the AIS library will be loaded automatically.

When AIS loaded successfully, you should see the AIS toolbar in AutoCAD.

---

## NOTE

AIS is NOT fuctionally completed. It is still in beta quality. So it is not user-friendly at this moment -- quite bumpy, actually. My apology.

## WORK ON SOURCE CODES

(Since AutoCAD v2015 the Visual LISP Editor was deprecated. Use Visual Studio Code instead, as recommended by Autodesk.)

If you would like to dig into the spaghetti source codes, do the following:

AutoCAD 2015 and before:
1. Launch Visual LISP Editor by selecting `Tools > AutoLISP > Visual LISP Editor`.
2. In the Visual LISP Editor, select `Project > Open Project...`, then browse to the AIS folder and locate the project file `source\AIS.prj`.
3. Now you should see all AIS files.
4. Before doing any re-compilation, be sure to set the Build Options:
- Select `Project > Project Properties...` to open Project properties panel.
- Switch to Build Options tab.
- Re-assign the **Fas** and **Tmp** directories so you get your binaries and object files separated and well managed.