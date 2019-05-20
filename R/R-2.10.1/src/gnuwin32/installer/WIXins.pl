#-*- perl -*-
# Copyright (C) 2001-9 R Development Core Team
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the GNU
# General Public License for more details.
#
# A copy of the GNU General Public License is available at
# http://www.r-project.org/Licenses/

# Send any bug reports to r-bugs@r-project.org

use Cwd;
use File::Find;
use Win32;

my $fn, $component, $path;
my $startdir=cwd();
my $RVER, $RVER0;
my $RW=$ARGV[0];
my $SRCDIR=$ARGV[1];

$SRCDIR =~ s+/+\\+g; # need DOS-style paths

## add to the target command line in the CmdParms function below

open ver, "< ../../../VERSION";
$RVER = <ver>;
close ver;
$RVER =~ s/\n.*$//;
$RVER =~ s/Under .*$/Pre-release/;
$RVER0 = $RVER;
$RVER0 =~ s/ .*$//;

my $sRW = Win32::GetShortPathName($SRCDIR);

my %uuids;

## we could use Win32::Guidgen, but it is not normally installed
## so it is easier to use C code.

my $nc = 900;
open guidfile, "<uuids" or die "Cannot open uuids\n";
while (<guidfile>) {
    chomp;
    $uuids{$nc++} = $_;
}
close guidfile;
$nc = 920;

open insfile, "> R.wxs" or die "Cannot open R.wxs\n";
print insfile <<END;
<?xml version="1.0" encoding="windows-1252"?>
<Wix xmlns="http://schemas.microsoft.com/wix/2003/01/wi">
  <Product Manufacturer="R Development Core Team" 
   Id="3AF9DA8E-B4DB-49B2-802D-A279667F44E5"
   Language="1033"
   Name="R $RVER for Windows"
   Version="$RVER" 
   UpgradeCode="309E663C-CA7A-40B9-8822-5D466F1E2AF9">
    <Package Id="????????-????-????-????-????????????" 
     Keywords="R $RVER for Windows Installer" 
     Description="R $RVER for Windows Installer" 
     Comments="R Language and Environment" 
     Manufacturer="R Development Core Team" 
     InstallerVersion="100" 
     Languages="1033" 
     Compressed="yes" 
     SummaryCodepage="1252" />
    <Media Id='1' Cabinet='Sample.cab' EmbedCab='yes' DiskPrompt="CD-ROM #1" />
    <Property Id='DiskPrompt' Value="R for Windows Installation [1]" />

    <Directory Id='TARGETDIR' Name='SourceDir'>

      <Directory Id='ProgramFilesFolder' Name='PFiles'>
        <Directory Id='R' Name='R'>
          <Directory Id='INSTALLDIR' Name = '$sRW' LongName='$RW'>
END

my $rgui, $rhelp;
my %comp;
open tfile, "<files.wxs" or die "Cannot open files.wxs\n";
while(<tfile>) {
    next unless /^        /;
    if(/<Component Id=\"([^\"]*)\"/) {
	$id = $1;
    }
    ## WiX 2.0.4221 uses 'src', 2.0.5805 uses 'Source'
    if(/<File Id=\"([^\"]*).* (src|Source)=\"([^\"]*)\"/) {
	$fn = $1;
	$src = $3;
	$src =~ s+.*\\$SRCDIR\\++;
	$src =~ s+\\+/+g;
	$comp{$src} = $id;
	$rgui = "$fn" if $src eq "bin/Rgui.exe";
	$rhelp = "$fn" if $src eq "doc/html/index.html";
    }
    if(/PUT-GUID-HERE/) {
	s/PUT-GUID-HERE/$uuids{$nc++}/;
    }
    print insfile "    ", $_;
}
close tfile;

$path="${SRCDIR}";chdir($path);
my %main;

find(\&listFiles, ".");

print insfile <<END;
          </Directory>
        </Directory>
      </Directory>

      <Directory Id="StartMenuFolder" Name="SMenu">
        <Directory Id="ProgramMenuFolder" Name="Programs">
          <Directory Id="RMENU" Name="R">
            <Component Id="shortcut0" 
             Guid="$uuids{910}" KeyPath="yes">
              <Shortcut Id="RguiStartMenuShortcut" Directory="RMENU" Name="R" 
               LongName="R $RVER" Target="[!$rgui]" 
               WorkingDirectory="INSTALLDIR" />
            </Component>
            <Component Id="shortcut1" 
             Guid="$uuids{911}" KeyPath="yes">
              <Shortcut Id="HelpStartMenuShortcut" Directory="RMENU" 
               Name="RHelp" LongName="R $RVER Help" Target="[!$rhelp]" 
               WorkingDirectory="INSTALLDIR" />
            </Component>
            <Component Id="shortcut2" 
             Guid="$uuids{912}" KeyPath="yes">
              <Shortcut Id="UninstallStartMenuShortcut" Directory="RMENU" 
               Name="RUninst" LongName="Uninstall R $RVER" 
               Target="[SystemFolder]\msiexec.exe" 
               Arguments="/x [ProductCode]" Icon="shell32.dll" 
               IconIndex="32" WorkingDirectory="INSTALLDIR" />
            </Component>
          </Directory>
        </Directory>
      </Directory>
      <Directory Id="DesktopFolder" Name="Desktop">
        <Component Id="desktopshortcut0" DiskId="1" Guid="$uuids{907}">
          <Shortcut Id="RguiDesktopShortcut" Directory="DesktopFolder" Name="R" LongName="R $RVER"
           WorkingDirectory="INSTALLDIR" Target="[!$rgui]" />
        </Component>
      </Directory>

      <Directory Id="AppDataFolder" Name="AppData">
        <Directory Id="Microsoft" Name="MS" LongName="Microsoft">
          <Directory Id="InternetExplorer" Name="IE" LongName="Internet Explorer">
            <Directory Id="QuickLaunch" Name="QLaunch" LongName="Quick Launch">
              <Component Id="quickshortcut0" DiskId="1" Guid="$uuids{908}">
                <Shortcut Id="RguiQuickShortcut" Directory="QuickLaunch" Name="R" LongName="R $RVER"
                 WorkingDirectory="INSTALLDIR" Target="[!$rgui]" />
              </Component>
            </Directory>
          </Directory>
        </Directory>
      </Directory>


      <Component Id="registry0" Guid="$uuids{900}">
        <Registry Id="RInstallPath" Root="HKMU" Key="Software\\R-core\\R" 
         Name="InstallPath" Type="string" KeyPath="yes" Value="[INSTALLDIR]" />
      </Component>
      <Component Id="registry1" Guid="$uuids{901}">
        <Registry Id="RCurrentVersion" Root="HKMU" Key="Software\\R-core\\R" 
         Name="Current Version" Type="string" KeyPath="yes" 
         Value="[ProductVersion]" />
      </Component>
      <Component Id="registry2" Guid="$uuids{902}">
        <Registry Id="RVerInstallPath" Root="HKMU" 
         Key="Software\\R-core\\R\\[ProductVersion]" Name="InstallPath"
         Type="string" KeyPath="yes" Value="[INSTALLDIR]" />
      </Component>
      <Component Id="registry3" Guid="$uuids{903}">
        <Registry Id="RData" Root="HKCR" Key=".RData" Type="string"
         KeyPath="yes" Value="RWorkspace" />
      </Component>
      <Component Id="registry4" Guid="$uuids{904}">
        <Registry Id="RWorkspace" Root="HKCR" Key="RWorkspace" Type="string" 
         KeyPath="yes" Value="R Workspace" />
      </Component>
      <Component Id="registry5" Guid="$uuids{905}">
        <Registry Id="RDataCommand" Root="HKCR" 
         Key="RWorkspace\\shell\\open\\command" Type="string" KeyPath="yes" 
         Value="&quot;[!$rgui]&quot; &quot;%1&quot;" />
      </Component>
      <Component Id="registry6" Guid="$uuids{906}">
        <Registry Id="RDataDefaultIcon" Root="HKCR" 
         Key="RWorkspace\\DefaultIcon" Type="string" KeyPath="yes" 
         Value="[!$rgui],0" />
      </Component>
    </Directory>

    <Feature Id="main" Title="Main Files" Description="Main Files" Level="1" 
     ConfigurableDirectory="INSTALLDIR"
     Display="expand" InstallDefault="local" AllowAdvertise="no" 
     Absent="disallow">
      <ComponentRef Id='registry0' />
      <ComponentRef Id='registry3' />
      <ComponentRef Id='registry4' />
      <ComponentRef Id='registry5' />
      <ComponentRef Id='registry6' />
END
    
foreach $n (sort values %main) {
    print insfile "      <ComponentRef Id='$n' />\n";
}

print insfile <<END;
    </Feature>
    <Feature Id="html" Title="HTML Help Files" Description="HTML Help Files" Level="1" InstallDefault="local" AllowAdvertise="no">
END

foreach $n (sort values %html) {
    print insfile "      <ComponentRef Id='$n' />\n";
}

print insfile <<END;
    </Feature>

    <Feature Id="manuals" Title="On-line (PDF) Manuals" 
     Description="On-line (PDF) Manual" Level="1"
     InstallDefault="local" AllowAdvertise="no">
END

foreach $n (sort values %manuals) {
    print insfile "      <ComponentRef Id='$n' />\n";
}

print insfile <<END;
    </Feature>

    <Feature Id="refman" Title="PDF Reference Manual" 
     Description="PDF Reference Manual" Level="1000"
     InstallDefault="local" AllowAdvertise="no">
END

foreach $n (sort values %refman) {
    print insfile "      <ComponentRef Id='$n' />\n";
}

print insfile <<END;
    </Feature>

    <Feature Id="libdocs" Title="Docs for Packages grid and Matrix" Description="Docs for Packages grid and Matrix" Level="1"
     InstallDefault="local" AllowAdvertise="no">
END

foreach $n (sort values %libdocs) {
    print insfile "      <ComponentRef Id='$n' />\n";
}

print insfile <<END;
    </Feature>

    <Feature Id="tcl" Title="Support Files for Package tcltk" Description="Support Files for Package tcltk" Level="1"
     InstallDefault="local" AllowAdvertise="no">
END

foreach $n (sort values %tcl) {
    print insfile "      <ComponentRef Id='$n' />\n";
}

print insfile <<END;
    </Feature>

    <Feature Id="trans" Title="Message Translations" Description="Messages translated to other languages" Level="1"
     InstallDefault="local" AllowAdvertise="no">
END

foreach $n (sort values %trans) {
    print insfile "      <ComponentRef Id='$n' />\n";
}

print insfile <<END;
    </Feature>

    <Feature Id="tests" Title="Test files" Description="Test files" Level="1"
     InstallDefault="local" AllowAdvertise="no">
END

foreach $n (sort values %tests) {
    print insfile "      <ComponentRef Id='$n' />\n";
}


## look up local Windows system32 directory
my $WINDOWS = Win32::GetFolderPath(Win32::CSIDL_SYSTEM);

print insfile <<END;
    </Feature>

    <Feature Id="shortcuts" Title="Shortcuts" Description="Shortcut install options" Level="1" InstallDefault="local"
     AllowAdvertise='no' Display="expand">
      <Feature Id='sshortcuts' Title='Start Menu Shortcuts' Description='Install Start menu shortcuts' Level='1'
       ConfigurableDirectory='RMENU' InstallDefault='local' AllowAdvertise='no'>
        <ComponentRef Id='shortcut0' />
        <ComponentRef Id='shortcut1' />
        <ComponentRef Id='shortcut2' />
      </Feature>
      <Feature Id='dshortcut' Title='Desktop Shortcut' Description='Install Desktop shortcut' Level='1'
       InstallDefault='local' AllowAdvertise='no'>
        <ComponentRef Id='desktopshortcut0' />
      </Feature>
      <Feature Id="qshortcut" Title="Quicklaunch Shortcut" Description="Install Quick Launch shortcut" Level="1000"
       InstallDefault="local" AllowAdvertise="no">
        <ComponentRef Id='quickshortcut0' />
      </Feature>
    </Feature>
    <Feature Id="registryversion" Title="Save Version in Registry"
     Description="Save the product version in the Registry" Level="1" InstallDefault="local" AllowAdvertise="no">
      <ComponentRef Id='registry1' />
      <ComponentRef Id='registry2' />
    </Feature>

    <UIRef Id="WixUI_Mondo" />
    <UIRef Id="WixUI_ErrorProgressText" />

    <Icon Id="shell32.dll" SourceFile="$WINDOWS\\shell32.dll" />

  </Product>
</Wix>
END

close insfile;

sub listFiles {
    $fn = $File::Find::name;
    if (!(-d $_)) {
	$_ = $dir = $fn;
	s+^./++;
	s+/+\\+g;
	$fn =~ s+.*/++g;

	if ($_ eq "doc\\manual\\R-FAQ.html"
		 || $_ eq "doc\\html\\rw-FAQ.html"
		 || $_ eq "share\\texmf\\Sweave.sty") {
	    $component = "main";
## FIXME: split out prebuilt HTML help pages
	} elsif (m/^doc\\html/
		 || m/^doc\\manual\\[^\\]*\.html/
		 || m/^library\\[^\\]*\\html/
		 || m/^library\\[^\\]*\\CONTENTS/
		 || $_ eq "library\\R.css") {
	    $component = "html";
	} elsif ($_ eq "doc\\manual\\refman.pdf") {
	    $component = "refman";
	} elsif (m/^doc\\manual/ && $_ ne "doc\\manual\\R-FAQ.pdf") {
	    $component = "manuals";
	} elsif (m/^library\\[^\\]*\\tests/) {
	    	$component = "tests";
	} elsif (m/^tests/) {
	    	$component = "tests";
	} elsif (m/^Tcl/) {
	    $component = "tcl";
	} elsif (m/^library\\grid\\doc/ || m/^library\\Matrix\\doc/) {
	    $component = "libdocs";
	} elsif (m/^share\\locale/ 
		 || m/^library\\[^\\]*\\po/) {
	    $component = "trans";
	} else {
	    $component = "main";
	}

	s+\\+/+g;
	$ncomp = $comp{$_};
	$main{$_} = $ncomp if $component eq "main";
	$html{$_} = $ncomp if $component eq "html";
	$manuals{$_} = $ncomp if $component eq "manuals";
	$refman{$_} = $ncomp if $component eq "refman";
	$libdocs{$_} = $ncomp if $component eq "libdocs";
	$tcl{$_} = $ncomp if $component eq "tcl";
	$trans{$_} = $ncomp if $component eq "trans";
	$tests{$_} = $ncomp if $component eq "tests";
    }
}
