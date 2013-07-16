%define version 0.4.0
%define release 1%{dist}

Summary: ToSCo - Tools for Scientific Computing
Name: tosco
Version: %{version}
Release: %{release}
License: GPLv3+
Group: Application/Misc
URL: http://www.gebrproject.com
Packager: Fabrício Matheus Gonçalves <fmatheus@gebrproject.com>
Source: %{name}-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}
BuildRequires: glib2-devel, gsl-devel, gcc-gfortran
BuildRequires: wget, gcc, stow, freeglut-devel, libXmu-devel, fakeroot
#BuildRequires: gebr-menus-su
%if 0%{?rhel} == 5
BuildRequires: openmotif-devel
%else
BuildRequires: lesstif-devel
%endif

%description
ToSCo is a set of scientific computing programs, mainly for processing geophysical data.

You can find more about the ToSCo project at http://code.google.com/p/tosco

This project is associated to GêBR project www.gebrproject.com.

%prep
%setup -q

%build
fakeroot ./gebr-su-install.sh -o /var/tmp/gebr-tmp-dir/cache
%configure CWPROOT=`./gebr-su-install.sh -i`
make

%install
rm -rf %{buildroot}
make install DESTDIR=%{buildroot}

%clean
rm -rf %{buildroot}

%files
%defattr(-, root, root)
/usr/share/*
/usr/bin/*

%changelog
