{*********************************************************************}
{ TADVSTRINGGRID component                                            }
{ for Delphi & C++Builder                                             }
{                                                                     }
{ written by TMS Software                                             }
{            copyright © 1996 - 2015                                  }
{            Email : info@tmssoftware.com                             }
{            Web : http://www.tmssoftware.com                         }
{*********************************************************************}

unit ASGReg;

interface

{$I TMSDEFS.INC}

uses
  Classes, Advgrid, BaseGrid, AsgCheck, AsgMemo, AdvSpin,
  AsgReplaceDialog, AsgFindDialog, AsgPrev, AsgPrint, AsgHTML, FrmCtrlLink,
  AdvGridCSVPager, AsgImport, AsgListB, AdvGridWorkbook
  {$IFDEF DELPHI7_LVL}
  , AdvGridLookupBar
  {$ENDIF}
  {$IFDEF DELPHIXE_LVL}
  , AdvGridPDFIO, AdvGridColPicker
  {$ENDIF}
  ;

{$R ASGREG.RES}

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Grids', [TAdvStringGrid]);
  RegisterComponents('TMS Grids', [TCapitalCheck]);
  RegisterComponents('TMS Grids', [TMemoEditLink]);
  RegisterComponents('TMS Grids', [TAdvGridUndoRedo]);
  RegisterComponents('TMS Grids', [TAdvGridReplaceDialog]);
  RegisterComponents('TMS Grids', [TAdvGridFindDialog]);
  RegisterComponents('TMS Grids', [TAdvPreviewDialog]);
  RegisterComponents('TMS Grids', [TAdvGridPrintSettingsDialog]);
  RegisterComponents('TMS Grids', [TAdvGridHTMLSettingsDialog]);
  RegisterComponents('TMS Grids', [TFormControlEditLink]);
  RegisterComponents('TMS Grids', [TAdvGridCSVPager]);
  RegisterComponents('TMS Grids', [TAdvGridCSVPager]);
  RegisterComponents('TMS Grids', [TAdvGridImportDialog]);
  RegisterComponents('TMS Grids', [TAdvGridHeaderList]);
  RegisterComponents('TMS Grids', [TAdvGridHeaderPopupList]);
  RegisterComponents('TMS Grids', [TAdvGridWorkbook]);
  {$IFDEF DELPHI7_LVL}
  RegisterComponents('TMS Grids', [TAdvGridLookupBar]);
  {$ENDIF}

  {$IFDEF DELPHIXE_LVL}
  RegisterComponents('TMS Grids', [TAdvGridColumnPicker]);
  RegisterComponents('TMS Grids', [TAdvGridPDFIO]);
  {$ENDIF}
end;

end.


   
               
             
       
               
               
               
  
