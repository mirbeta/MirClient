{***************************************************************************}
{ TTaskDialog component                                                     }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2006 - 2012                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit TaskDialogDE;

interface

{$I TMSDEFS.INC}

uses
  Classes, Graphics, Comctrls, Windows, Forms, TypInfo, Dialogs, ExtCtrls,
  Controls, ExtDlgs, TaskDialog, DesignIntf, DesignEditors, ContNrs;

type

  TTaskDialogEditor = class(TDefaultEditor)
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

implementation

{ TTaskDialogEditor }

procedure TTaskDialogEditor.ExecuteVerb(Index: Integer);
var
  AppIsParent: boolean;
begin
  inherited;
  case Index of
    0:
    begin
      AppIsParent := TCustomAdvTaskDialog(Component).ApplicationIsParent;
      TCustomAdvTaskDialog(Component).ApplicationIsParent := true;
      TCustomAdvTaskDialog(Component).Execute;
      TCustomAdvTaskDialog(Component).ApplicationIsParent := AppIsParent;
    end;
  end;
end;

function TTaskDialogEditor.GetVerb(Index: Integer): string;
begin
  case Index of
  0: Result := 'Preview';
  end;
end;

function TTaskDialogEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;



end.
