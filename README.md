# moog17scat
MOOG 2017 version with scattering

MOOG is a radiative transfer code for stellar abundances written by Chris Sneden.
http://www.as.utexas.edu/~chris/moog.html

The publicly available version of MOOG does not treat scattering properly for blue, metal-poor stars. This was fixed in 2011 by Jennifer Sobeck (Sobeck et al. 2011, AJ 141, 175),
and her version was widely distributed. A proper treatment of scattering is important to accurately compute abundances
of blue lines in metal-poor stars.
However, MOOG has since experienced several upgrades, with a big update coming in 2014. Scattering was not propagated to this version.

Here, I have taken the publicly available version of MOOG (from Feb 2017)
and patched in the code written by Jennifer Sobeck.
It can be turned on by setting "scat 1" in the configuration (batch.par) files.

I take absolutely no credit (or responsibility) for this code, as others have done all the heavy lifting! But I hope it saves someone out there some time.

IMPORTANT UPDATE: Aug 7, 2017.
I mistakenly introduced a bug causing problems with abfind. It is now fixed.

WARNING:
Blends.f and Ewfind.f were not modified in the 2011 version I have. I may be mistaken, but it does not appear that they use Sobeck's scattering. I will eventually fix this, but this likely affected anything that used the blends driver for bluer lines (<4500A) in metal-poor stars.
UPDATE: I have learned that some versions of the scattering code did have Blends and Ewfind fixed. If you would like to check your version, you can do so by looking to see if "cdcalc_JS" is in Blends.f or Ewfind.f. If so, then you are probably good!

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
