{********************************************************************
TCALCEDIT & TCALCLABEL component
for Delphi 2.0, 3.0, 4.0, 5.0 & C++Builder 1.0, 3.0, 4.0, 5.0
version 1.0

written by TMS Software
           copyright © 2000
           Email : info@tmssoftware.com
           Web : http://www.tmssoftware.com
{********************************************************************}

unit calcereg;

interface

uses
 calcedit,classes;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS', [TCalcLabel,TCalcEdit]);
end;




end.

