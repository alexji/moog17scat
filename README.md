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


WARNING:
Blends.f and Ewfind.f were not modified in the 2011 version I have. I may be mistaken, but it does not appear that they use Sobeck's scattering. I will eventually fix this, but this likely affected anything that used the blends driver for bluer lines (<4500A) in metal-poor stars.
