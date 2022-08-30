{********************************************************************
TLAYEREDFORM component
for Delphi & C++Builder

written by TMS Software
           copyright © 2012
           Email : info@tmssoftware.com
           Web : http://www.tmssoftware.com
{********************************************************************}

unit lfreg;

{$I TMSDEFS.INC}
interface

uses
 layeredform,classes;


procedure Register;

implementation

procedure Register;
begin
 RegisterComponents('TMS', [TLayeredForm]);
end;



end.

