      implicit real*16(a-h,o-z)
      parameter(n=100,ns=100)
      dimension zcal(0:ns,0:n),zd(0:ns),ad(0:ns) 
      dimension advs(50),zvals(50)
      open(10,file='series-center-data')
      open(12,file='series-coef-data')
      open(13,file='inv-tab-data')
      pi=4.*atan(1.0)
      fact=sqrt(2.0*pi)
      nset= 50
      tiny=1e-7
      
      do i=0,ns
      read(10,*)zd(i),ad(i)
      enddo
      do i=0,ns
      read(12,*) (zcal(i,j),j=0,n)
      enddo
      delv=0.5/nset
      do iv=1,nset
      adv=iv*delv
      if(iv.eq.nset) adv=iv*delv-tiny 
      i=-1
11      i=i+1
      if(adv.le.ad(i)) then
      ifix=i-1
      go to 10
      endif
      go to 11
10    a0=ad(ifix)*fact
      z0=zd(ifix)
      aval=adv*fact
      zval=0.0
      do k=0,n
      zval=zval+zcal(ifix,k)*(aval-a0)**k
      enddo
      advs(iv)=adv
      zvals(iv)=zval
      enddo
      do iv=1,25
      write(13,15)advs(iv),zvals(iv),advs(iv+25),zvals(iv+25)
      enddo    
15   format(1x,f10.8,1x,'&',1x,E16.10,1x,'&',1x,f10.8,1x,'&',1x,E16.10,'\\')
     write(*,*)adv,zval
     
     stop
     end
      
      
     
     
     

