
      subroutine makeplot (lscreen)
c******************************************************************************
c     This subroutine does the plot-package-specific commands to begin
c     a plot, then calls the specific plot drawing routine, then ends the
c     plot.
c******************************************************************************

      implicit real*8 (a-h,o-z)
      include 'Atmos.com'
      include 'Pstuff.com'
      integer sm_device, lscreen, nchars


cc  open the plot device: screen terminal
c      if (plotroutine(1:4) .eq. 'term') then
c         if (sm_device(smterm) .lt. 0) then
c            write (array,1001) smterm
c            istat = ivwrite(lscreen+1,1,array,79)
c            write (nf1out,1007) array(1:79)
c            stop
c         endif
c      endif
c
c
cc  open the plot device: hardcopy sent to printer
c      if (plotroutine(1:4) .eq. 'hard') then
c         if     (plotroutine(6:9) .eq. 'land') then
c            if     (sm_device('postland') .lt. 0) then
c               write (array,1002)
c               istat = ivwrite(lscreen+1,1,array,34)
c               write (nf1out,1007) array(1:34)
c               stop
c            endif
c         elseif (plotroutine(6:9) .eq. 'port') then
c            if (sm_device('postport') .lt. 0) then
c               write (array,1009)
c               istat = ivwrite(lscreen+1,1,array,34)
c               write (nf1out,1007) array(1:34)
c               write (nf1out,1009)
c               stop
c            endif
c         endif
c      endif
c
c
cc  open the plot device: postscript file
c      if (plotroutine(1:4) .eq. 'file') then
c         if (f5out .eq. 'optional_output_file') then
c            array = 'Give the file name for the POSTSRIPT plot image: '
c            nchars = 49
c            call getasci (nchars,maxline)
c            f5out = chinfo(1:nchars)
c         else
c            nchars = 80
c            call getcount (nchars,f5out)
c         endif
c         if     (plotroutine(6:9) .eq. 'land') then
c            if (nchars .lt. 10) then
c               write (errmess,1003) nchars
c            else
c               write (errmess,1004) nchars
c            endif
c         elseif (plotroutine(6:9) .eq. 'port') then
c            if (nchars .lt. 10) then
c               write (errmess,1005) nchars
c            else
c               write (errmess,1006) nchars
c            endif
c         endif
c         write (array,errmess) f5out(1:nchars)
c         if (sm_device(array(1:nchars+13)) .lt. 0) then
c            write (nf1out,1007) array(1:nchars+9)
c            istat = ivwrite(lscreen+1,1,array,nchars+9)
c            stop
c         endif
c      endif
c
c
cc  issue standard beginning commands
c      call sm_graphics
c      call sm_erase
c
c
cc  call the routine that makes the desired plot
c      if     (plotroutine(11:14) .eq. 'cog ') then
c         call cogplot
c      elseif (plotroutine(11:14) .eq. 'abun') then
c         call abunplot
c      elseif (plotroutine(11:14) .eq. 'spec') then
c         call specplot
c      elseif (plotroutine(11:14) .eq. 'bin ') then
c         call binplot
c      elseif (plotroutine(11:14) .eq. 'flux') then
c         call fluxplot
c      endif
c
c
cc  issue standard ending commands; exit normally
c      if (plotroutine(1:4) .eq. 'file') then
c         f5out = 'optional_output_file'
c      endif
c      call sm_gflush
c      if (plotroutine(1:4).eq.'hard' .or. 
c     .    plotroutine(1:4).eq.'file') call sm_hardcopy
c      call sm_alpha
      return


c*****format statements
1001  format ('DEVICE OPENING ERROR FOR:',a54)
1002  format ('DEVICE OPENING ERROR FOR: postland')
1009  format ('DEVICE OPENING ERROR FOR: postport')
1007  format (a80)
1003  format ('(13hpostlandfile ,a',i1,'$)')
1004  format ('(13hpostlandfile ,a',i2,'$)')
1005  format ('(13hpostportfile ,a',i1,'$)')
1006  format ('(13hpostportfile ,a',i2,'$)')


      end


