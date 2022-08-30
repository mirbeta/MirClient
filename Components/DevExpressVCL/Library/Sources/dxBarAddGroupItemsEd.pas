{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressBars choose group item form                       }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSBARS AND ALL ACCOMPANYING VCL  }
{   CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.                  }
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

unit dxBarAddGroupItemsEd;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  UITypes,
{$ENDIF}
  Types, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls, ExtCtrls, StdCtrls,
  dxCommon, dxBar, cxClasses, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxLayoutContainer, dxForms,
  dxLayoutControlAdapters, dxLayoutControl, dxLayoutLookAndFeels, cxContainer, cxEdit, cxListBox, Menus, cxButtons;

type

  { TfrmAddGroupItems }

  TfrmAddGroupItems = class(TdxForm)
    btnCancel: TcxButton;
    btnOk: TcxButton;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutControl1: TdxLayoutControl;
    dxLayoutControl1Group_Root: TdxLayoutGroup;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    lbGroupItems: TcxListBox;

    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure lbGroupItemsDrawItem(AControl: TcxListBox; ACanvas: TcxCanvas; AIndex: Integer; ARect: TRect; AState: TOwnerDrawState);
    procedure lbGroupItemsMeasureItem(AControl: TcxListBox; AIndex: Integer; var Height: Integer);
  strict private
    FGroup: TdxBarGroup;

    procedure SetGroup(Value: TdxBarGroup);
  public
    GroupItems: TList;

    property Group: TdxBarGroup read FGroup write SetGroup;
  end;

function dxBarChooseGroupItem(AGroup: TdxBarGroup; AGroupItems: TList): Boolean;

implementation

{$R *.DFM}

function dxBarChooseGroupItem(AGroup: TdxBarGroup; AGroupItems: TList): Boolean;
begin
  with TfrmAddGroupItems.Create(nil) do
  try
    Group := AGroup;
    GroupItems := AGroupItems;
    Result := ShowModal = mrOk;
  finally
    Free;
  end;
end;

{ TfrmAddGroupItems }

procedure TfrmAddGroupItems.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  I: Integer;
begin
  if ModalResult = mrOk then
    for I := 0 to lbGroupItems.Count - 1 do
    begin
      if lbGroupItems.Selected[I] then
        GroupItems.Add(lbGroupItems.Items.Objects[I]);
    end;
end;

procedure TfrmAddGroupItems.lbGroupItemsDrawItem(AControl: TcxListBox;
  ACanvas: TcxCanvas; AIndex: Integer; ARect: TRect; AState: TOwnerDrawState);

  procedure DrawLine(const S: string; const R: TRect; ABackgroundColor, ATextColor: TColor);
  begin
    ACanvas.Font.Color := ATextColor;
    ACanvas.FillRect(R, ABackgroundColor);
    cxExtTextOut(ACanvas.Handle, S, Point(R.Left + 2, R.Top + 1), R, ETO_OPAQUE);
  end;

  procedure CalculateLineColors(APainter: TcxCustomLookAndFeelPainter; AItem: TdxBarComponent; out ABackgroundColor, ATextColor: TColor);
  begin
    if Group.IndexOf(AItem) >= 0 then
    begin
      ATextColor := cxGetActualColor(APainter.DefaultEditorTextColor(True), clGrayText);
      ABackgroundColor := cxGetActualColor(APainter.DefaultEditorBackgroundColor(True), clWindow);
    end
    else
      if odSelected in AState then
      begin
        ATextColor := cxGetActualColor(APainter.DefaultSelectionTextColor, clHighlightText);
        ABackgroundColor := cxGetActualColor(APainter.DefaultSelectionColor, clHighlight);
      end
      else
      begin
        ATextColor := cxGetActualColor(APainter.DefaultEditorTextColor(False), clWindowText);
        ABackgroundColor := cxGetActualColor(APainter.DefaultEditorBackgroundColor(False), clWindow);
      end;
  end;

var
  ABackgroundColor: TColor;
  AItem: TdxBarComponent;
  ALineRect: TRect;
  APainter: TcxCustomLookAndFeelPainter;
  ATextColor: TColor;
  AValue: string;
begin
  AValue := AControl.Items[AIndex];
  APainter := AControl.Style.LookAndFeel.Painter;
  if (AValue <> '') and (AValue[1] = '~') then
  begin
    DrawLine(Copy(AValue, 2, MaxInt), ARect, APainter.DefaultHeaderBackgroundColor, APainter.DefaultHeaderBackgroundTextColor);
    Exit;
  end;

  AItem := TdxBarComponent(AControl.Items.Objects[AIndex]);
  ALineRect := ARect;
  if AItem is TdxBarItem then
    ALineRect.Right := (ALineRect.Left + ALineRect.Right) div 2 - 1;
  CalculateLineColors(APainter, AItem, ABackgroundColor, ATextColor);
  DrawLine('   ' + AValue, ALineRect, ABackgroundColor, ATextColor);

  if AItem is TdxBarItem then
  begin
    ALineRect.Left := ALineRect.Right;
    Inc(ALineRect.Right);
    ACanvas.FillRect(ALineRect, ATextColor);
    ALineRect.Left := ALineRect.Right;
    ALineRect.Right := ARect.Right;
    DrawLine(AItem.Name, ALineRect, ABackgroundColor, ATextColor);
  end;
end;

procedure TfrmAddGroupItems.lbGroupItemsMeasureItem(AControl: TcxListBox; AIndex: Integer; var Height: Integer);
begin
  Height := cxTextHeight(AControl.Style.Font) + cxTextOffset;
end;

procedure TfrmAddGroupItems.SetGroup(Value: TdxBarGroup);
var
  List: TList;
  I, J: Integer;
begin
  FGroup := Value;
  Font := FGroup.BarManager.Font;
  with lbGroupItems.Items do
  begin
    List := TList.Create;
    BeginUpdate;
    try
      if Group.BarManager.GroupCount > 1 then
      begin
        Add('~< Groups >');
        with Group.BarManager do
          for I := 0 to GroupCount - 1 do
            if Groups[I] <> Group then
              AddObject(Groups[I].Name, Groups[I]);
      end;
      for I := 0 to Group.BarManager.Categories.Count - 1 do
      begin
        J := Group.BarManager.GetItemsByCategory(I, List);
        if J <> 0 then
        begin
          Add('~' + Group.BarManager.Categories[I]);
          for J := 0 to J - 1 do
            with TdxBarItem(List[J]) do
              AddObject(Caption, List[J]);
        end;
      end;
    finally
      EndUpdate;
      List.Free;
    end;
  end;
end;

end.
