{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxfmDfnStl;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Menus, ImgList, dxCore, cxLookAndFeelPainters,
  cxButtons, cxControls, cxContainer, cxListBox, cxGraphics, dxPSESys,
  dxPSForm, dxPgsDlg, cxLookAndFeels, cxEdit, dxLayoutContainer, dxLayoutControlAdapters,
  cxClasses, dxLayoutControl, dxLayoutLookAndFeels, cxImageList;

type

  { TdxfmDefinePrintStyles }

  TdxfmDefinePrintStyles = class(TCustomdxPSForm)
    btnClose: TcxButton;
    btnCopy: TcxButton;
    btnEdit: TcxButton;
    btnHelp: TcxButton;
    btnReset: TcxButton;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    ilPrintStyles: TcxImageList;
    lblPrintStyles: TdxLayoutItem;
    lbxPrintStyles: TcxListBox;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    liClose: TdxLayoutItem;
    liHelp: TdxLayoutItem;
    miClear: TMenuItem;
    miCopy: TMenuItem;
    miEdit: TMenuItem;
    miLine1: TMenuItem;
    miReset: TMenuItem;
    pmPrintStyles: TPopupMenu;

    procedure ClearClick(Sender: TObject);
    procedure CopyClick(Sender: TObject);
    procedure EditClick(Sender: TObject);
    procedure lbxPrintStylesClick(Sender: TObject);
    procedure lbxPrintStylesDrawItem(AControl: TcxListBox; ACanvas: TcxCanvas; AIndex: Integer; ARect: TRect; AState: TOwnerDrawState);
    procedure pmPrintStylesPopup(Sender: TObject);
    procedure ResetClick(Sender: TObject);
  private
    FBtnCopyCaptions: array[Boolean] of string;
    FBtnDelCaptions: array[Boolean] of string;
    FPrevClassStyle: DWORD;
    FPreviewBtnClicked: Boolean;
    FPrevWindowProc: TWndMethod;
    FPrintBtnClicked: Boolean;
    FStyleManager: TdxPrintStyleManager;
    FSubscriber: TdxEventSubscriber;

    procedure SetStyleManager(Value: TdxPrintStyleManager);

    procedure AddPrintStyle(AClonedIndex: Integer);
    procedure FillRestSpace(DC: hDC);
    procedure Initialize;
    procedure LoadStrings;
    procedure RefreshStyleList;
    procedure StyleListChanged(Sender: TObject);
    procedure UpdateControlsState;

    procedure ListBoxWndProc(var Message: TMessage);
    procedure RestoreWndProc(AListBox: TListBox);
    procedure SubstWindowProc(AListBox: TListBox);
  protected
    procedure BeforeConstruction; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;

    property PreviewBtnClicked: Boolean read FPreviewBtnClicked;
    property PrintBtnClicked: Boolean read FPrintBtnClicked;
    property StyleManager: TdxPrintStyleManager read FStyleManager write SetStyleManager;
  end;

procedure dxDefinePrintStylesDlg(AStyleManager: TdxPrintStyleManager;
  const ATitle: string; var APreviewBtnClicked, APrintBtnClicked: Boolean);
implementation

{$R *.DFM}

uses
  Registry, dxPSRes, dxPSEngn, dxPSEvnt, dxPSPopupMan, dxPSGlbl, dxPSUtl, dxPSImgs;

procedure dxDefinePrintStylesDlg(AStyleManager: TdxPrintStyleManager;
  const ATitle: string; var APreviewBtnClicked, APrintBtnClicked: Boolean);
var
  Dialog: TdxfmDefinePrintStyles;
begin
  if AStyleManager = nil then
  begin
    APreviewBtnClicked := False;
    APrintBtnClicked := False;
    Exit;
  end;
  Dialog := TdxfmDefinePrintStyles.Create(nil);
  try
    Dialog.Caption := ATitle;
    Dialog.StyleManager := AStyleManager;
    try
      Dialog.Execute;
    finally
      Dialog.StyleManager := nil;
      APreviewBtnClicked := Dialog.PreviewBtnClicked;
      APrintBtnClicked := Dialog.PrintBtnClicked;
    end;
  finally
    Dialog.Free;
  end;
end;

function MessageWarning(const Message: string): Boolean;
begin
  MessageBeep(MB_ICONEXCLAMATION);
  Result := IDOK = Application.MessageBox(PChar(Message),
   PChar(Application.Title), MB_OKCANCEL or MB_ICONEXCLAMATION);
end;

{ TfmdxDefinePrintStyles }

constructor TdxfmDefinePrintStyles.Create(AOwner: TComponent);
begin
  HelpContext := dxhcDefinePrintStyleDlg;
  inherited;
  dxLoadImageListFromResources(ilPrintStyles, IDIL_DXPSDESIGNWINDOWMENU);

  FSubscriber := TdxStyleListChangedSubscriber.Create([TdxSMStyleListChangedEvent]);
  TdxStyleListChangedSubscriber(FSubscriber).OnStyleListChanged := StyleListChanged;
  SubstWindowProc(lbxPrintStyles.InnerListBox);
  dxPSPopupMenuController.RegisterControl(lbxPrintStyles);
  LoadStrings;
end;

destructor TdxfmDefinePrintStyles.Destroy;
begin
  dxPSPopupMenuController.UnregisterControl(lbxPrintStyles);
  RestoreWndProc(lbxPrintStyles.InnerListBox);
  FreeAndNil(FSubscriber);
  inherited;
end;

function TdxfmDefinePrintStyles.Execute: Boolean;
begin
  Initialize;
  ShowModal;
  Result := True;
  if (StyleManager <> nil) and (lbxPrintStyles.ItemIndex <> -1) then
    StyleManager.CurrentStyleIndex := lbxPrintStyles.ItemIndex;
end;

procedure TdxfmDefinePrintStyles.BeforeConstruction;
begin
  inherited;
  Options := Options + [foSizeableDialog];
end;

procedure TdxfmDefinePrintStyles.SetStyleManager(Value: TdxPrintStyleManager);
begin
  if FStyleManager <> Value then
  begin
    FStyleManager := Value;
    if FStyleManager <> nil then
    begin
      Caption := Value.Title;
      if Value.HelpContext <> 0 then
        HelpContext := Value.HelpContext;
    end;
  end;
end;

procedure TdxfmDefinePrintStyles.AddPrintStyle(AClonedIndex: Integer);
var
  Style: TBasedxPrintStyle;
  Result: Boolean;
begin
  Result := False;
  Style := StyleManager.BeginClone(AClonedIndex);
  if Style = nil then Exit;
  try
    Result := Style.PageSetup(0, True, True, FPreviewBtnClicked, FPrintBtnClicked);
    if Result or FPreviewBtnClicked or FPrintBtnClicked then
    begin
      lbxPrintStyles.Items.AddObject(Style.StyleCaption, Style);
      lbxPrintStyles.ItemIndex := lbxPrintStyles.Items.IndexOfObject(Style);
    end;
  finally
    StyleManager.EndClone(Style);
    if not Result then Style.Free;
  end;

  if PreviewBtnClicked or PrintBtnClicked then
    ModalResult := mrOK
  else
    if lbxPrintStyles.ItemIndex <> -1 then
      lbxPrintStylesClick(lbxPrintStyles);
end;

procedure TdxfmDefinePrintStyles.FillRestSpace(DC: hDC);
var
  R: TRect;
begin
  with lbxPrintStyles.InnerListBox do
  begin
    SendMessage(Handle, LB_GETITEMRECT, Items.Count - 1, LPARAM(@R));
    if R.Bottom < ClientHeight then
    begin
      R := Rect(0, R.Bottom, ClientWidth, ClientHeight);
      FillRect(DC, R, GetSysColorBrush(COLOR_WINDOW));
    end;
  end;
end;

procedure TdxfmDefinePrintStyles.Initialize;
begin
  TdxStyleListChangedSubscriber(FSubscriber).StyleListChanged(StyleManager);
  liHelp.Visible := HelpContext <> 0;
  if liHelp.Visible then
    BorderIcons := BorderIcons + [biHelp];
  ActiveControl := lbxPrintStyles;
  UpdateControlsState;
end;

procedure TdxfmDefinePrintStyles.LoadStrings;
begin
  Caption := cxGetResourceString(@sdxDefinePrintStylesCaption);
  lblPrintStyles.Caption := cxGetResourceString(@sdxDefinePrintStylesTitle);
  btnEdit.Caption := cxGetResourceString(@sdxBtnEdit);
  btnCopy.Caption := cxGetResourceString(@sdxBtnCopy);
  FBtnDelCaptions[False] := cxGetResourceString(@sdxBtnDelete);
  FBtnDelCaptions[True] := cxGetResourceString(@sdxBtnReset);
  FBtnCopyCaptions[False] := cxGetResourceString(@sdxBtnNew);
  FBtnCopyCaptions[True] := cxGetResourceString(@sdxBtnCopy);
  btnClose.Caption := cxGetResourceString(@sdxBtnClose);
  btnHelp.Caption := cxGetResourceString(@sdxBtnHelp);
  miEdit.Caption := cxGetResourceString(@sdxBtnEdit);
  miCopy.Caption := cxGetResourceString(@sdxBtnCopy);
  miClear.Caption := cxGetResourceString(@sdxClear);
end;

procedure TdxfmDefinePrintStyles.RefreshStyleList;
var
  SavedStyle, Style: TBasedxPrintStyle;
  I, Index: Integer;
begin
  if FStyleManager <> nil then
  begin
    if lbxPrintStyles.ItemIndex <> -1 then
      SavedStyle := TBasedxPrintStyle(lbxPrintStyles.Items.Objects[lbxPrintStyles.ItemIndex])
    else
      SavedStyle := FStyleManager.CurrentStyle;

    with lbxPrintStyles.Items do
    begin
      BeginUpdate;
      try
        Clear;
        for I := 0 to FStyleManager.Count - 1 do
        begin
          Style := FStyleManager[I];
          AddObject(Style.StyleCaption, Style);
        end;
      finally
        EndUpdate;
      end;
    end;

    if lbxPrintStyles.Items.Count > 0 then
    begin
      Index := lbxPrintStyles.Items.IndexOfObject(SavedStyle);
      if Index = -1 then
        Index := FStyleManager.CurrentStyleIndex;
      lbxPrintStyles.ItemIndex := Index;
      lbxPrintStylesClick(lbxPrintStyles);
    end;
  end;
end;

procedure TdxfmDefinePrintStyles.StyleListChanged(Sender: TObject);
begin
  if Sender = StyleManager then
  begin
    RefreshStyleList;
    UpdateControlsState;
  end;
end;

procedure TdxfmDefinePrintStyles.UpdateControlsState;
begin
  btnEdit.Enabled := lbxPrintStyles.ItemIndex > -1;
  btnReset.Enabled := lbxPrintStyles.ItemIndex > -1;
  btnCopy.Caption := FBtnCopyCaptions[lbxPrintStyles.Items.Count > 0];
  if lbxPrintStyles.Items.Count > 0 then
    miCopy.ImageIndex := 1
  else
    miCopy.ImageIndex := 3;
end;

procedure TdxfmDefinePrintStyles.ListBoxWndProc(var Message: TMessage);
begin
  if Message.Msg = WM_ERASEBKGND then
  begin
    with TWMEraseBkgnd(Message) do
      FillRestSpace(DC);
    Message.Result := 1;
  end
  else
    FPrevWindowProc(Message);
end;

procedure TdxfmDefinePrintStyles.RestoreWndProc(AListBox: TListBox);
begin
  AListBox.WindowProc := FPrevWindowProc;
  SetClassLong(AListBox.Handle, GCL_STYLE, FPrevClassStyle);
end;

procedure TdxfmDefinePrintStyles.SubstWindowProc(AListBox: TListBox);
begin
  AListBox.HandleNeeded;
  FPrevClassStyle := GetClassLong(AListBox.Handle, GCL_STYLE);
  SetClassLong(AListBox.Handle, GCL_STYLE, FPrevClassStyle or CS_HREDRAW);
  FPrevWindowProc := AListBox.WindowProc;
  AListBox.WindowProc := ListBoxWndProc;
end;

procedure TdxfmDefinePrintStyles.EditClick(Sender: TObject);
var
  AStyle: TBasedxPrintStyle;
begin
  with lbxPrintStyles do
    AStyle := TBasedxPrintStyle(Items.Objects[ItemIndex]);
  if AStyle.PageSetup(0, True, True, FPreviewBtnClicked, FPrintBtnClicked) then
    StyleListChanged(StyleManager);
  if PreviewBtnClicked or PrintBtnClicked then
    ModalResult := mrOK;
end;

procedure TdxfmDefinePrintStyles.CopyClick(Sender: TObject);
begin
  AddPrintStyle(lbxPrintStyles.ItemIndex);
end;

procedure TdxfmDefinePrintStyles.ResetClick(Sender: TObject);
var
  Style: TBasedxPrintStyle;
  S: string;
begin
  Style := TBasedxPrintStyle(lbxPrintStyles.Items.Objects[lbxPrintStyles.ItemIndex]);

  if not Style.BuiltIn then
  begin
    S := Format(cxGetResourceString(@sdxDefinePrintStylesWarningDelete), [Style.StyleCaption]);
    if MessageWarning(S) then Style.Free;
  end
  else
    Style.RestoreDefaults;
  UpdateControlsState;
end;

procedure TdxfmDefinePrintStyles.pmPrintStylesPopup(Sender: TObject);
begin
  with lbxPrintStyles do
  begin
    miEdit.Enabled := ItemIndex > -1;
    miReset.Enabled := ItemIndex > -1;
    miClear.Enabled := StyleManager.NonBuiltInsExists;

    if ItemIndex > -1 then
      miReset.Caption := FBtnDelCaptions[TBasedxPrintStyle(Items.Objects[ItemIndex]).BuiltIn];

    miCopy.Caption := FBtnCopyCaptions[Items.Count > 0];
  end;
end;

procedure TdxfmDefinePrintStyles.lbxPrintStylesClick(Sender: TObject);
var
  PrintStyle: TBasedxPrintStyle;
begin
  with TcxListBox(Sender) do
  begin
    PrintStyle := nil;
    if ItemIndex <> -1 then
      PrintStyle := StyleManager[ItemIndex];
    btnReset.Caption := FBtnDelCaptions[(PrintStyle <> nil) and PrintStyle.BuiltIn];
    miReset.Caption := btnReset.Caption;
    if (PrintStyle <> nil) and PrintStyle.BuiltIn then
    begin
      miReset.ShortCut := TShortCut(0);
      miReset.ImageIndex := -1;
    end
    else
    begin
      miReset.ShortCut := ShortCut(VK_DELETE, []);
      miReset.ImageIndex := 2;
    end;
    UpdateControlsState;
  end;
end;

procedure TdxfmDefinePrintStyles.ClearClick(Sender: TObject);
begin
  if MessageWarning(cxGetResourceString(@sdxDefinePrintStylesWarningClear)) then
    StyleManager.DeleteNonBuiltIns;
end;

procedure TdxfmDefinePrintStyles.lbxPrintStylesDrawItem(
  AControl: TcxListBox; ACanvas: TcxCanvas; AIndex: Integer; ARect: TRect;
  AState: TOwnerDrawState);
begin
  dxPSDrawStyleItem(TBasedxPrintStyle(lbxPrintStyles.Items.Objects[AIndex]),
    AControl.InnerListBox, AIndex, AState, ARect, True, False, ScaleFactor);
  if AIndex = lbxPrintStyles.Items.Count - 1 then
    FillRestSpace(ACanvas.Handle);
end;

end.
