{***********************************************************************}
{ TADVLISTVIEW, TDBADVLISTVIEW component                                }
{ for Delphi & C++Builder                                               }
{ version 1.6                                                           }
{                                                                       }
{ written by                                                            }
{   TMS Software                                                        }
{   copyright © 1998-2006                                               }
{   Email : info@tmssoftware.com                                        }
{   Web : http://www.tmssoftware.com                                    }
{***********************************************************************}
{$IFNDEF TMSPERSONAL}

{$DEFINE DBADVLST}

{$ENDIF}

unit AlvRegDe;

interface
{$I TMSDEFS.INC}
uses
  Advlistv, AlvDE
  {$IFDEF DBADVLST}
  ,DBAdvLst,DB,DBAlvDE
  {$ENDIF}
  ,Classes
{$IFDEF DELPHI6_LVL}
  ,DesignIntf, DesignEditors
{$ELSE}
  ,DsgnIntf
{$ENDIF}
  ;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponentEditor(TAdvListView,TAdvListViewEditor);
  {$IFDEF DBADVLST}
  RegisterPropertyEditor(TypeInfo(string),TListViewField,'FieldName',TLvFieldNameProperty);
  {$ENDIF}
end;



end.

