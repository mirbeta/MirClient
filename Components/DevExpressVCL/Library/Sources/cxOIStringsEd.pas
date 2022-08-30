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
unit cxOIStringsEd;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls, Menus,
  dxCore, cxLookAndFeels, cxLookAndFeelPainters, cxButtons, cxControls, cxContainer, cxEdit, cxTextEdit, cxMemo,
  cxGraphics, dxLayoutcxEditAdapters, dxLayoutControlAdapters, dxLayoutContainer, dxLayoutLookAndFeels, cxClasses,
  dxLayoutControl, dxForms;

type
  TcxfmStringsEditor = class(TdxForm)
    btnCancel: TcxButton;
    btnOK: TcxButton;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    Label1: TdxLayoutLabeledItem;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    Memo1: TcxMemo;

    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Memo1PropertiesChange(Sender: TObject);
  private
    FSizeGripBounds: TRect;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DoShow; override;
  end;

  PcxStringsEditorDlgData = ^TcxStringsEditorDlgData;
  TcxStringsEditorDlgData = record
    Caption: string;
    LookAndFeel: TcxLookAndFeel;
    Text: string;
  end;

function cxShowStringsEditor(const AData: PcxStringsEditorDlgData): Boolean;

implementation

uses
  Types, dxThemeConsts, dxThemeManager, dxUxTheme, cxVGridConsts;

{$R *.DFM}

function cxShowStringsEditor(const AData: PcxStringsEditorDlgData): Boolean;
var
  Form: TcxfmStringsEditor;
begin
  Form := TcxfmStringsEditor.Create(nil);
  with Form do
  try
    Caption := AData.Caption;
    Memo1.Text := AData.Text;
    dxLayoutCxLookAndFeel1.LookAndFeel.MasterLookAndFeel := AData.LookAndFeel;
    btnOK.Caption := cxGetResourceString(@cxSvgOKCaption);
    btnCancel.Caption := cxGetResourceString(@cxSvgCancelCaption);
    Result := Form.ShowModal = mrOK;
    if Result then
      AData^.Text := Memo1.Text;
  finally
    Free;
  end;
end;

procedure TcxfmStringsEditor.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := WS_EX_DLGMODALFRAME or WS_EX_WINDOWEDGE;
end;

procedure TcxfmStringsEditor.CreateWnd;
begin
  inherited CreateWnd;
  SendMessage(Self.Handle, WM_SETICON, 1, 0);
end;

procedure TcxfmStringsEditor.DoShow;
begin
  inherited DoShow;
  Memo1PropertiesChange(nil);
end;

procedure TcxfmStringsEditor.Memo1PropertiesChange(Sender: TObject);
begin
  Label1.Caption := IntToStr(Memo1.InnerControl.Perform(EM_GETLINECOUNT, 0, 0)) + ' line(s)';
end;

procedure TcxfmStringsEditor.WMNCHitTest(var Message: TWMNCHitTest);
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    if Message.Result in [HTCLIENT, HTRIGHT, HTBOTTOM]  then
      if PtInRect(FSizeGripBounds, GetMouseCursorPos) then
        Message.Result := HTBOTTOMRIGHT;
  end;
end;

procedure TcxfmStringsEditor.FormPaint(Sender: TObject);
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

procedure TcxfmStringsEditor.FormResize(Sender: TObject);
begin
  FSizeGripBounds := cxGetWindowRect(Self);
  FSizeGripBounds.Left := FSizeGripBounds.Right - GetSystemMetrics(SM_CXVSCROLL);
  FSizeGripBounds.Top := FSizeGripBounds.Bottom - GetSystemMetrics(SM_CYHSCROLL);
  Invalidate;
end;

end.
