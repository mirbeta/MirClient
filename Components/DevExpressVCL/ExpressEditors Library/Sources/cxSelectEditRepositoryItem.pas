{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit cxSelectEditRepositoryItem;

{$I cxVer.inc}

interface

uses
  Types, Windows, SysUtils, Classes, Controls, Forms, StdCtrls, cxClasses, cxEdit,
  ExtCtrls;

type
  TcxSelectRepositoryItemSetup = record
    TopIndex: Integer;
    Selected: Integer;
    List: TcxRegisteredClasses;
    Pos: TPoint;
  end;

  TcxSelectRepositoryItem = class(TForm)
    lbItems: TListBox;
    Panel1: TPanel;
    lbHint: TLabel;
    Label1: TLabel;
    btOk: TButton;
    btCancel: TButton;
    pnlClient: TPanel;
    pnlBar: TPanel;
    procedure lbItemsClick(Sender: TObject);
    procedure lbItemsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FList: TcxRegisteredClasses;
    procedure Setup(const AData: TcxSelectRepositoryItemSetup);
    procedure SaveSetup(var AData: TcxSelectRepositoryItemSetup);
  end;

function GetEditRepositoryItemClass(
  var ASetupData: TcxSelectRepositoryItemSetup): TcxEditRepositoryItemClass;

implementation

{$R *.dfm}

function GetEditRepositoryItemClass(
  var ASetupData: TcxSelectRepositoryItemSetup): TcxEditRepositoryItemClass;
var
  ASelectRepositoryItem: TcxSelectRepositoryItem;
begin
  Result := nil;
  ASelectRepositoryItem := TcxSelectRepositoryItem.Create(nil);
  try
    ASelectRepositoryItem.Setup(ASetupData);
    if ASelectRepositoryItem.ShowModal = mrOk then
      Result := TcxEditRepositoryItemClass(
        ASelectRepositoryItem.lbItems.Items.Objects[ASelectRepositoryItem.lbItems.ItemIndex]);
    ASelectRepositoryItem.SaveSetup(ASetupData);
  finally
    ASelectRepositoryItem.Release;
  end;
end;

procedure TcxSelectRepositoryItem.lbItemsClick(Sender: TObject);
begin
  btOk.Enabled := lbItems.ItemIndex <> -1;
  if btOk.Enabled then
    lbHint.Caption := FList.GetHintByClass(
      TcxEditRepositoryItemClass(lbItems.Items.Objects[lbItems.ItemIndex]))
  else
    lbHint.Caption := '';
end;

procedure TcxSelectRepositoryItem.lbItemsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (ssDouble in Shift) and (mbLeft = Button) and
    (lbItems.ItemAtPos(Point(X, Y), True) <> -1) then ModalResult := mrOk;
end;

procedure TcxSelectRepositoryItem.Setup(const AData: TcxSelectRepositoryItemSetup);
var
  I: Integer;
begin
  with AData do
  begin
    FList := List;
    for I := 0 to List.Count - 1 do
      lbItems.Items.AddObject(List.Descriptions[I], TObject(List[I]));
    if (Pos.X = -1) and (Pos.Y = -1) then Position := poScreenCenter
    else
    begin
      Left := Pos.X;
      Top := Pos.Y;
    end;
    lbItems.TopIndex := TopIndex;
    if (Selected = -1) or ((Selected > 0) and (Selected < List.Count)) then
      lbItems.ItemIndex := Selected;
    lbItemsClick(nil);
  end;
end;

procedure TcxSelectRepositoryItem.SaveSetup(var AData: TcxSelectRepositoryItemSetup);
begin
  with AData do
  begin
    Pos := Point(Left, Top);
    Selected := lbItems.ItemIndex;
    TopIndex := lbItems.TopIndex;
  end;
end;

end.

