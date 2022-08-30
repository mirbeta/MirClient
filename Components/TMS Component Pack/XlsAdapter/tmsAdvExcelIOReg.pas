unit tmsAdvExcelIOReg;
{$INCLUDE ..\FLXCOMPILER.INC}
{$INCLUDE ..\FLXCONFIG.INC}

interface
uses Classes, tmsAdvGridExcel;
procedure Register;

implementation
procedure Register;
begin
{$IFDEF FLEXCELADVSTRINGGRID}
  RegisterComponents('TMS Grids', [TAdvGridExcelIO]);
{$ENDIF}
end;

end.
