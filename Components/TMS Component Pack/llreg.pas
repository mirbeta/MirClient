{********************************************************************
TLISTLINK component
for Delphi & C++Builder

written by TMS Software
           copyright © 1998-2012
           Email : info@tmssoftware.com
           Web : http://www.tmssoftware.com
{********************************************************************}

unit llreg;

{$I TMSDEFS.INC}
interface

uses
 listlink,classes;


procedure Register;

implementation

procedure Register;
begin
 RegisterComponents('TMS', [TListLink]);
end;



end.

