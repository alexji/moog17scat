# moog14scat
MOOG 2014 version with scattering

MOOG is a radiative transfer code for stellar abundances written by Chris Sneden.
http://www.as.utexas.edu/~chris/moog.html

The publicly available version of MOOG does not treat scattering properly. This was fixed in 2011 by Jennifer Sobeck,
and her version was widely distributed. A proper treatment of scattering is important to accurately compute abundances
of blue lines in metal-poor stars.
However, MOOG has since experienced several upgrades, with a big update coming in 2014. Scattering was not propagated to this version.

Here, I have taken the publicly available version of MOOG (which was available from Jul 2014-Feb 2017; the 2017 version is mostly the same)
and patched in the code written by Jennifer Sobeck.
It can be turned on by setting "scat 1" in the configuration (batch.par) files.

I take absolutely no credit (or responsibility) for this code, as others have done all the heavy lifting! But I hope it saves someone out there some time.
