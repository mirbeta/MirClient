{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MSICS_FPC_Dsgn;

interface

uses
  MSI_DsgnIntf, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('MSI_DsgnIntf', @MSI_DsgnIntf.Register);
end;

initialization
  RegisterPackage('MSICS_FPC_Dsgn', @Register);
end.
