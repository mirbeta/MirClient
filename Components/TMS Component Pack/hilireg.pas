{********************************************************************
THintList component
for Delphi & C++Builder

written by TMS Software
           copyright © 1998-2012
           Email : info@tmssoftware.com
           Web : http://www.tmssoftware.com
{********************************************************************}

unit Hilireg;

{$I TMSDEFS.INC}

interface

uses
 HintList, Classes;


procedure Register;

implementation

procedure Register;
begin
 RegisterComponents('TMS', [THintList]);
end;



end.

