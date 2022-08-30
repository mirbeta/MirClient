{*******************************************************************}
{ TDBADVSTRINGGRID component                                        }
{ for Delphi & C++Builder                                           }
{                                                                   }
{ written by                                                        }
{    TMS Software                                                   }
{    copyright © 1999-2012                                          }
{    Email : info@tmssoftware.com                                   }
{    Web : http://www.tmssoftware.com                               }
{                                                                   }
{*******************************************************************}

unit dbgregde;

interface

{$I TMSDEFS.INC}

uses
  Classes, DBAdvGrd, DBAsgDE
  , HtmlDE
{$IFDEF DELPHI6_LVL}
  , DesignIntf, DesignEditors, ContNrs
{$ELSE}
  , DsgnIntf
{$ENDIF}
  ;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponentEditor(TDBAdvStringGrid,TDBAdvStringGridEditor);
  RegisterPropertyEditor(TypeInfo(string),TStringGridField,'FieldName',TSgFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(TStringList), TDBAdvStringGrid, 'HTMLTemplate', THTMLTextProperty);
end;

end.
