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

unit AlvReg;

interface
{$I TMSDEFS.INC}
uses
  Classes, Advlistv
  {$IFDEF DBADVLST}
  , DBAdvLst,DB
  {$ENDIF}
  ;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS', [TAdvListView]); 
  {$IFDEF DBADVLST}
  RegisterComponents('TMS', [TDBAdvListView]);
  {$ENDIF}
end;



end.

