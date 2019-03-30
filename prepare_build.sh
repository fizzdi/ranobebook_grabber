#!/bin/bash

package_name=$1
version=$2
release=$3

mkdir SOURCES
mkdir "SOURCES/$package_name-$version"
rsync -r --exclude 'SOURCES' --exclude '.git' ./* "SOURCES/$package_name-$version"

mkdir RPMS
mkdir BUILD
mkdir BUILDROOT

mkdir SPECS
cp "$package_name.spec" SPECS/

mkdir SRPMS


cd SOURCES/
tar -zcvf "$package_name-$version.tar.gz" "$package_name-$version/"
cd ../
rpmbuild --define "_topdir `pwd`" -ba "SPECS/$package_name.spec" --define "version $version" --define "release $release"

