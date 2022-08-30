{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxRichEdit.Dialogs.ControlHelper;

interface

{$I cxVer.inc}
{$I dxRichEditControl.inc}

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ELSE}
  Windows,
{$ENDIF}
  Classes, SysUtils, Graphics, Controls, cxEdit, cxListBox, cxTextEdit,
  dxCoreClasses,
  dxRichEdit.Utils.BatchUpdateHelper;

{$IFNDEF DELPHIXE2}
const
  vkPrior = VK_PRIOR;
  vkNext = VK_NEXT;
  vkUp = VK_UP;
  vkDown = VK_DOWN;
{$ENDIF}

type

  { TdxCustomRichEditFontHelper }

  TdxCustomControlHelper = class(TcxIUnknownObject, IdxBatchUpdateable, IdxBatchUpdateHandler)
  protected
    FBatchUpdateHelper: TdxBatchUpdateHelper;
    FDeferredUpdateControl: Boolean;

    procedure UpdateControl; virtual;
    procedure UpdateControlCore; virtual;
    procedure SubscribeControlsEvents; virtual;
    procedure UnsubscribeControlsEvents; virtual;

    { IdxBatchUpdateable }
    function GetIsUpdateLocked: Boolean;
    function GetBatchUpdateHelper: TdxBatchUpdateHelper;
    { IdxBatchUpdateHandler }
    procedure OnFirstBeginUpdate; virtual;
    procedure OnBeginUpdate; virtual;
    procedure OnEndUpdate; virtual;
    procedure OnLastEndUpdate; virtual;
    procedure OnCancelUpdate; virtual;
    procedure OnLastCancelUpdate; virtual;

  public
    constructor Create;
    destructor Destroy; override;

    { IdxBatchUpdateable }
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    procedure CancelUpdate; virtual;
    property IsUpdateLocked: Boolean read GetIsUpdateLocked;
    property BatchUpdateHelper: TdxBatchUpdateHelper read GetBatchUpdateHelper;

    property DefferedUpdateControl: Boolean read FDeferredUpdateControl write FDeferredUpdateControl;
  end;

  { TdxCustomRichEditFontListBoxHelper }

  TdxCustomListBoxHelper = class(TdxCustomControlHelper)
  private
    FEditControl: TcxTextEdit;
    FListControl: TcxListBox;
    FNearestIndex: Integer;
    FOnChange: TNotifyEvent;
    FOldEditControlChange: TNotifyEvent;
  protected
    procedure DoChange;
    procedure DoUpdateEditControl;
    procedure EditControlChange(Sender: TObject);
    procedure EditControlExit(Sender: TObject);
    procedure EditControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListControlClick(Sender: TObject);
    procedure SubscribeControlsEvents; override;
    procedure UnsubscribeControlsEvents; override;

    property EditControl: TcxTextEdit read FEditControl;
    property ListControl: TcxListBox read FListControl;
  public
    constructor Create(AEditControl: TcxTextEdit; AListControl: TcxListBox);
    function Validate: Boolean; virtual;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

uses
  Math, cxClasses;

{ TdxCustomControlHelper }

procedure TdxCustomControlHelper.BeginUpdate;
begin
  FBatchUpdateHelper.BeginUpdate;
end;

procedure TdxCustomControlHelper.CancelUpdate;
begin
  FBatchUpdateHelper.CancelUpdate;
end;

constructor TdxCustomControlHelper.Create;
begin
  inherited Create;
  FBatchUpdateHelper := TdxBatchUpdateHelper.Create(Self);
end;

destructor TdxCustomControlHelper.Destroy;
begin
  FBatchUpdateHelper.Free;
  inherited Destroy;
end;

procedure TdxCustomControlHelper.EndUpdate;
begin
  FBatchUpdateHelper.EndUpdate;
end;

function TdxCustomControlHelper.GetBatchUpdateHelper: TdxBatchUpdateHelper;
begin
  Result := FBatchUpdateHelper;
end;

function TdxCustomControlHelper.GetIsUpdateLocked: Boolean;
begin
  Result := FBatchUpdateHelper.IsUpdateLocked;
end;

procedure TdxCustomControlHelper.OnBeginUpdate;
begin
end;

procedure TdxCustomControlHelper.OnCancelUpdate;
begin
end;

procedure TdxCustomControlHelper.OnEndUpdate;
begin
end;

procedure TdxCustomControlHelper.OnFirstBeginUpdate;
begin
  UnsubscribeControlsEvents;
  FDeferredUpdateControl := False;
end;

procedure TdxCustomControlHelper.OnLastCancelUpdate;
begin
  SubscribeControlsEvents;
end;

procedure TdxCustomControlHelper.OnLastEndUpdate;
begin
end;

procedure TdxCustomControlHelper.SubscribeControlsEvents;
begin
end;

procedure TdxCustomControlHelper.UnsubscribeControlsEvents;
begin
end;

procedure TdxCustomControlHelper.UpdateControl;
begin
  if IsUpdateLocked then
    FDeferredUpdateControl := True
  else
  begin
    UnsubscribeControlsEvents;
    UpdateControlCore;
    SubscribeControlsEvents;
  end;
end;

procedure TdxCustomControlHelper.UpdateControlCore;
begin
end;

{ TdxCustomRichEditFontListBoxHelper }

constructor TdxCustomListBoxHelper.Create(AEditControl: TcxTextEdit; AListControl: TcxListBox);
begin
  inherited Create;
  FEditControl := AEditControl;
  FListControl := AListControl;
  FOldEditControlChange := FEditControl.Properties.OnChange;
end;

procedure TdxCustomListBoxHelper.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TdxCustomListBoxHelper.DoUpdateEditControl;
begin
  if FListControl.ItemIndex >= 0 then
    FEditControl.EditValue := FListControl.Items[FListControl.ItemIndex];
end;

procedure TdxCustomListBoxHelper.EditControlExit(Sender: TObject);
begin
  DoChange;
end;

procedure TdxCustomListBoxHelper.EditControlChange(Sender: TObject);
var
  AEditValue: string;
  I: Integer;
begin
  if Assigned(FOldEditControlChange) then
    FOldEditControlChange(Sender);
  if not FListControl.Focused then
  begin
    FNearestIndex := 0;
    AEditValue := LowerCase(FEditControl.Text);
    if AEditValue = '' then
      Exit;
    I := FListControl.Items.IndexOf(AEditValue);
    if I >= 0 then
    begin
      FListControl.ItemIndex := I;
      FNearestIndex := I;
    end
    else
    begin
      for I := 0 to FListControl.Items.Count - 1 do
        if Pos(AEditValue, LowerCase(FListControl.Items[I])) = 1 then
        begin
          FListControl.TopIndex := I;
          FNearestIndex := I;
          Break;
        end;
      FListControl.ItemIndex := -1;
    end;
  end;
end;

procedure TdxCustomListBoxHelper.EditControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

  function ItemsInPage: Integer;
  begin
    Result := IfThen(FListControl.ItemHeight > 0, FListControl.Height div FListControl.ItemHeight - 1, 1);
  end;

begin
  case Key of
    vkPrior:
      FListControl.ItemIndex := IfThen(FListControl.ItemIndex >= 0,
        Max(FListControl.ItemIndex - ItemsInPage, 0), Min(FNearestIndex, FListControl.Count - 1));
    vkNext:
      FListControl.ItemIndex := IfThen(FListControl.ItemIndex >= 0,
        Min(FListControl.ItemIndex + ItemsInPage, FListControl.Count - 1), Min(FNearestIndex, FListControl.Count - 1));
    vkUp:
      FListControl.ItemIndex := IfThen(FListControl.ItemIndex >= 0,
        Max(FListControl.ItemIndex - 1, 0), Min(FNearestIndex, FListControl.Count - 1));
    vkDown:
      FListControl.ItemIndex := IfThen(FListControl.ItemIndex >= 0,
        Min(FListControl.ItemIndex + 1, FListControl.Count - 1), Min(FNearestIndex, FListControl.Count - 1));
    else
      Exit;
  end;
  CallNotify(FListControl.OnClick, FListControl);
end;

procedure TdxCustomListBoxHelper.ListControlClick(Sender: TObject);
begin
  if FListControl.ItemIndex < 0 then
    Exit;
  DoUpdateEditControl;
  DoChange;
end;

procedure TdxCustomListBoxHelper.SubscribeControlsEvents;
begin
  inherited SubscribeControlsEvents;
  FEditControl.Properties.OnChange := EditControlChange;
  FEditControl.OnExit := EditControlExit;
  FEditControl.OnKeyDown := EditControlKeyDown;
  FListControl.OnClick := ListControlClick;
end;

procedure TdxCustomListBoxHelper.UnsubscribeControlsEvents;
begin
  inherited UnsubscribeControlsEvents;
  FEditControl.Properties.OnChange := FOldEditControlChange;
  FEditControl.OnExit := nil;
  FEditControl.OnKeyDown := nil;
  FListControl.OnClick := nil;
end;

function TdxCustomListBoxHelper.Validate: Boolean;
begin
  Result := FEditControl.ValidateEdit(True);
  if not Result then
    FEditControl.SetFocus;
end;

end.
