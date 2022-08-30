{********************************************************************
TPROPSCROLLBAR component
for Delphi 2.0, 3.0, 4.0, 5.0 & C++Builder 1.0, 3.0, 4.0
version 1.2

written by TMS Software
           copyright © 1998-1999
           Email : info@tmssoftware.com
           Web : http://www.tmssoftware.com
{********************************************************************}


unit scrlreg;

interface

uses
 propscrl,classes;

procedure Register;

implementation

procedure Register;
begin
 RegisterComponents('TMS', [TPropScrollBar]);
end;


end.
 