# moog17scat
MOOG 2017 version with scattering

# UPDATE: Sep 1, 2022
Jen Sobeck has a new repository with updates to the code and more robustness checks.
I recommend new users use her version as it will likely be much better maintained than this repository.
https://github.com/jsobeck/MOOG-SCAT_basic_git

## UPDATE: Nov 16, 2021
I have updated `Synspec.f` following the comment below. I think it is better to change this than not.

Additionally, I have increased the hardcoded number of isotopes (it used to be 20, now it is 200).
Previously you would have been safe up to 40 isotopes if you did not use the MOOG GUI.

A question often asked is the differences between this and [Chris Sneden's](https://www.as.utexas.edu/~chris/moog.html) most recent [Nov 2019](https://www.as.utexas.edu/~chris/MOOGNOV2019.tar.gz) code.
In short, the recent update improved treatment of molecules (H2O, CO2), a linelist I/O setting that stopped automatic conversion of gf to loggf, and synthetic stellar populations (see [the documentation](https://www.as.utexas.edu/~chris/codes/WRITEnov2019.pdf)).
However, it does not have the updates described below, most notably not including scattering (despite the documentation saying otherwise).
So, if you are interested in similar science as me (abundances in metal-poor giants in optical wavelengths) I would recommend sticking to moog17scat.


## Comment: Apr 20, 2021.
I have been bothered by some of the "ringing" that happens at the ~0.005 normalized flux level when scattering is turned on, which didn't occur before in the 2011 Sobeck version. Some digging shows that in `Synspec.f` line 60, the wavelength differences threshold to recompute continuum quantities used to be 0.0001 instead of 0.001.
Changing this does appear to remove the ringing features, but it require many more calls to the continuum source function, so it's slower. For low S/N spectra this does not matter, but for high S/N spectra (around 100) you will want to change this parameter and recompile when computing synthetic spectra.

## IMPORTANT UPDATE: Sep 13, 2018.
A bug was introduced with the "trudamp" parameter in the 2014 refactoring.
It was turned on by default, rather than kept off.
This is now fixed to be off by default.
If you would like to turn trudamp on, use "trudamp 1" in the parameters.

There is also a typo bug in Trudamp.f that has been fixed for Ca3933 (thanks to Rana Ezzeddine for identifying the bug).

The bug only affected these specific lines (see Damping.f and Trudamp.f):
* Ca II (3933, 8498, 8542, 8662)
* CH (3693)
* Ca I (6717, 6318, 6343, 6361)
* Ca I autoionization (6318, 6343, 6361)

## IMPORTANT UPDATE: Aug 7, 2017.
I mistakenly introduced a bug causing problems with abfind. It is now fixed.

## MOOG
MOOG is a radiative transfer code for stellar abundances written by Chris Sneden.
http://www.as.utexas.edu/~chris/moog.html

The publicly available version of MOOG does not treat scattering properly for blue, metal-poor stars. This was fixed in 2011 by Jennifer Sobeck (Sobeck et al. 2011, AJ 141, 175),
and her version was widely distributed. A proper treatment of scattering is important to accurately compute abundances
of blue lines in metal-poor stars.
However, MOOG has since experienced several upgrades, with a big update coming in 2014. Scattering was not propagated to this version.

Here, I have taken the publicly available version of MOOG (from Feb 2017)
and patched in the code written by Jennifer Sobeck.
It can be turned on by setting "scat 1" in the configuration (batch.par) files.

WARNING:
Blends.f and Ewfind.f were not modified in the 2011 version I have. I may be mistaken, but it does not appear that they use Sobeck's scattering. I will eventually fix this, but this likely affected anything that used the blends driver for bluer lines (<4500A) in metal-poor stars.
UPDATE: I have learned that some versions of the scattering code did have Blends and Ewfind fixed. If you would like to check your version, you can do so by looking to see if "cdcalc_JS" is in Blends.f or Ewfind.f. If so, then you are probably good!

## Citation
If you use this code in a paper, please cite Sneden 1973 and Sobeck et al. 2011, AJ 141, 175.
I would also appreciate if you include a link to this page in a footnote (www.github.com/alexji/moog17scat).

## Installation Instructions
Instructions copied from http://www.as.utexas.edu/~chris/moog.html
* Download/extract or clone the repository (green button in top right)
  * If you are not sure which one to do, I recommend you clone. That way future updates or bugfixes can be downloaded easily.
  * https://help.github.com/articles/cloning-a-repository/
* Edit the `moogpath` variable in `Moog.f` and `Moogsilent.f` to suit your download location
* Compile MOOG with `make -f Makefile.xxx` where `xxx` is the setup that you want.
  * You will have to have a fortran compiler. `g77` is the one assumed by MOOG by default.
  * For Mac OS X, and old version of `g77` can be downloaded here: http://hpc.sourceforge.net/
  * However, `gfortran` is really the only fortran compiler supported on newer Mac laptops. So I recommend you use this instead.
  * I have edited `Makefile.maclapsilent` to work with `gfortran`. See that file if you want to see what compiler flags you should set to use `gfortran`.
* Ensure this new MOOG or MOOGSILENT is your default in your path.
  * You can edit your `.cshrc`, `.bashrc`, and/or `.bash_profile` to add this new directory.
  * For example, my `.bash_profile` has `export PATH="/Users/alexji/lib/moog17scat:$PATH"`
  * Check this by opening a new terminal and typing `which MOOGSILENT`. It should point to the correct place.

Some notes from Vini Placco about installing on Catalina.
```
I just compiled moog17scat on a fresh macOS Catalina installation. I had two small hiccups:

The gfortran I used at first came from gcc 10.2.0 (from homebrew). When I used this version, I got a few compilation errors on OpacHydrogen.f. Instead of messing with the code, I installed an earlier version of gfortran, which I knew worked:

brew install gcc@5

Then, on the Makefile.macdesk I have:

FC = /usr/local/Cellar/gcc\@5/5.5.0_4/bin/gfortran-5 -Wall -fno-range-check -w -ff2c
This compiled everything without errors or warnings.

I also had an error on the AQLIB library, and replacing: -L$(AQLIB) -laquaterm with -L$(AQLIB) -framework AquaTerm solved the problem.
```

Alex notes (2021-06-15):
```
The error Vini finds is in OpacHydrogen.f where a scalar is being passed into a variable that expects an array. Older fortran compilers had no problem with this (would throw a warning) but newer ones throw an error.

In newer versions of gfortran (v10 and above) you can get around this by adding this flag to the fortran compilation.
-fallow-argument-mismatch

e.g. change Makefile.maclapsilent to have:
FC = gfortran -fno-range-check -w -fallow-argument-mismatch

Thanks to Jandrie Rodriguez for helping debug this.
```
