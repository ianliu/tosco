%define version 0.2.0
%define release 1%{dist}

Summary: ToSCo - Tools for Scientific Computing
Name: tosco
Version: %{version}
Release: %{release}
License: GPLv3+
Group: Application/Misc
URL: http://www.gebrproject.com
Packager: Ian Liu Rodrigues <ian.liu@gebrproject.com>
Source: %{name}-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}

%description
ToSCo is a set of scientific computing programs, mainly for processing geophysical data.

You can find more about the ToSCo project at http://code.google.com/p/tosco

This project is associated to GêBR project www.gebrproject.com.

%prep
%setup -q

%build
%configure
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
