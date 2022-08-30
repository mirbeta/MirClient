{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressVerticalGrid                                      }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSVERTICALGRID AND ALL           }
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
unit cxOIPictureEd;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ExtDlgs, Buttons, StdCtrls, Menus, ComCtrls,
  dxCore, cxLookAndFeelPainters, cxControls, cxContainer, cxEdit, cxImage, cxButtons, cxLookAndFeels, cxGraphics,
  cxClasses, dxLayoutContainer, dxLayoutControl, dxLayoutcxEditAdapters, dxLayoutControlAdapters, dxLayoutLookAndFeels,
  dxForms;

type
  TcxfmPictureEditor = class(TdxForm)
    Bevel1: TBevel;
    btnCancel: TcxButton;
    btnClear: TcxButton;
    btnCopy: TcxButton;
    btnLoad: TcxButton;
    btnOk: TcxButton;
    btnPaste: TcxButton;
    btnSave: TcxButton;
    dxLayoutControl1: TdxLayoutControl;
    dxLayoutControl1Group_Root: TdxLayoutGroup;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    Image: TcxImage;

    procedure btnClearClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnPasteClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FChained: Boolean;
    FGraphicFilter: string;
    FNextWindow: HWND;
    FSizeGripBounds: TRect;

    function GetSaveGraphicFilter: string;
    procedure ForwardMessage(var Message: TMessage);
    procedure WMChangeCBChain(var Message: TWMChangeCBChain); message WM_CHANGECBCHAIN;
    procedure WMDrawClipboard(var Message: TMessage); message WM_DRAWCLIPBOARD;
    procedure WMNCDestroy(var Message: TWMNCDestroy); message WM_NCDESTROY;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWindowHandle; override;
    function HasPicture: Boolean;
    procedure UpdateButtons;
  end;

  PcxPictureEditorDlgData = ^TcxPictureEditorDlgData;
  TcxPictureEditorDlgData = record
    Caption: string;
    ClipboardFormat: Word;
    GraphicFilter: string;
    LookAndFeel: TcxLookAndFeel;
    Picture: TPicture;
  end;

function cxShowPictureEditor(const AData: PcxPictureEditorDlgData): Boolean;

implementation

{$R *.DFM}

uses
  Types, ClipBrd, cxVGridConsts, cxEditConsts, dxThemeConsts, dxThemeManager, dxUxTheme;

function cxShowPictureEditor(const AData: PcxPictureEditorDlgData): Boolean;
var
  Form: TcxfmPictureEditor;
begin
  Form := TcxfmPictureEditor.Create(nil);
  with Form do
  try
    dxLayoutCxLookAndFeel1.LookAndFeel.MasterLookAndFeel := AData.LookAndFeel;
    btnCopy.Caption := cxGetResourceString(@cxSMenuItemCaptionCopy);
    btnPaste.Caption := cxGetResourceString(@cxSMenuItemCaptionPaste);
    btnClear.Caption := cxGetResourceString(@cxSMenuItemCaptionDelete);
    btnLoad.Caption := cxGetResourceString(@cxSMenuItemCaptionLoad);
    btnSave.Caption := cxGetResourceString(@cxSMenuItemCaptionSave);
    btnOK.Caption := cxGetResourceString(@cxSvgOKCaption);
    btnCancel.Caption := cxGetResourceString(@cxSvgCancelCaption);
    Image.Picture := AData^.Picture;
    Caption := AData^.Caption;
    FGraphicFilter := AData^.GraphicFilter;
    Image.ClipboardFormat := AData^.ClipboardFormat;
    UpdateButtons;
    Result := ShowModal = mrOK;
    if Result then
      AData^.Picture.Assign(Image.Picture);
  finally
    Free;
  end;
end;

procedure TcxfmPictureEditor.CreateWnd;
begin
  inherited CreateWnd;
  if Handle <> 0 then
  begin
    FNextWindow := SetClipboardViewer(Handle);
    FChained := True;
  end;
  SendMessage(Self.Handle, WM_SETICON, 1, 0);
end;

procedure TcxfmPictureEditor.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := WS_EX_DLGMODALFRAME or WS_EX_WINDOWEDGE;
end;

procedure TcxfmPictureEditor.DestroyWindowHandle;
begin
  if FChained then
  begin
    ChangeClipboardChain(Handle, FNextWindow);
    FChained := False;
  end;
  FNextWindow := 0;
  inherited DestroyWindowHandle;
end;

procedure TcxfmPictureEditor.FormPaint(Sender: TObject);
var
  ATheme: TdxTheme;
  R: TRect;
begin
  R := ClientRect;
  R.Left := R.Right - GetSystemMetrics(SM_CXVSCROLL);
  R.Top := R.Bottom - GetSystemMetrics(SM_CYHSCROLL);
  if btnOk.LookAndFeel.NativeStyle and AreVisualStylesAvailable([totScrollBar]) then
  begin
    ATheme := OpenTheme(totScrollBar);
    DrawThemeBackground(ATheme, Canvas.Handle, SBP_SIZEBOX, SZB_RIGHTALIGN, @R);
  end
  else
    DrawFrameControl(Canvas.Handle, R, DFC_SCROLL, DFCS_SCROLLSIZEGRIP);
end;

procedure TcxfmPictureEditor.FormResize(Sender: TObject);
begin
  FSizeGripBounds := cxGetWindowRect(Self);
  FSizeGripBounds.Left := FSizeGripBounds.Right - GetSystemMetrics(SM_CXVSCROLL);
  FSizeGripBounds.Top := FSizeGripBounds.Bottom - GetSystemMetrics(SM_CYHSCROLL);
  Invalidate;
end;

procedure TcxfmPictureEditor.ForwardMessage(var Message: TMessage);
begin
  if FNextWindow <> 0 then
    with Message do
      SendMessage(FNextWindow, Msg, WParam, LParam);
end;

procedure TcxfmPictureEditor.WMChangeCBChain(var Message: TWMChangeCBChain);
begin
  if Message.Remove = FNextWindow then
    FNextWindow := Message.Next
  else ForwardMessage(TMessage(Message));
  inherited;
end;

procedure TcxfmPictureEditor.WMDrawClipboard(var Message: TMessage);
begin
  UpdateButtons;
  ForwardMessage(Message);
  inherited;
end;

procedure TcxfmPictureEditor.WMNCDestroy(var Message: TWMNCDestroy);
begin
  if FChained then
  begin
    ChangeClipboardChain(Handle, FNextWindow);
    FChained := False;
    FNextWindow := 0;
  end;
  inherited;
end;

procedure TcxfmPictureEditor.WMNCHitTest(var Message: TWMNCHitTest);
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    if Message.Result in [HTCLIENT, HTRIGHT, HTBOTTOM]  then
      if PtInRect(FSizeGripBounds, GetMouseCursorPos) then
        Message.Result := HTBOTTOMRIGHT;
  end;
end;

function TcxfmPictureEditor.HasPicture: Boolean;
begin
  Result := (Image.Picture.Graphic <> nil) and not Image.Picture.Graphic.Empty;
end;

procedure TcxfmPictureEditor.UpdateButtons;
begin
  btnSave.Enabled := HasPicture;
  btnCopy.Enabled := HasPicture;
  btnPaste.Enabled := Clipboard.HasFormat(Image.ClipboardFormat);
  btnClear.Enabled := HasPicture;
end;

procedure TcxfmPictureEditor.btnLoadClick(Sender: TObject);
begin
  Image.Properties.CustomFilter := FGraphicFilter;
  Image.LoadFromFile;
  UpdateButtons;
end;

procedure TcxfmPictureEditor.btnSaveClick(Sender: TObject);
begin
  Image.Properties.CustomFilter := GetSaveGraphicFilter;
  Image.SaveToFile;
  UpdateButtons;
end;

procedure TcxfmPictureEditor.btnClearClick(Sender: TObject);
begin
  Image.Clear;
  UpdateButtons;
end;

procedure TcxfmPictureEditor.btnCopyClick(Sender: TObject);
begin
  Image.CopyToClipboard;
  UpdateButtons;
end;

procedure TcxfmPictureEditor.btnPasteClick(Sender: TObject);
begin
  Image.PasteFromClipboard;
  UpdateButtons;
end;

function TcxfmPictureEditor.GetSaveGraphicFilter: string;
begin
  if Image.Picture.Graphic = nil then
    Result := GraphicFilter(TGraphic)
  else
    Result := GraphicFilter(TGraphicClass(Image.Picture.Graphic.ClassType));
end;

end.
