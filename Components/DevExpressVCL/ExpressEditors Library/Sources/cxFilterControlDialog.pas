{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressFilterControl                                     }
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
unit cxFilterControlDialog;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Menus, ExtCtrls,
  dxCore, cxLookAndFeelPainters, cxGraphics, cxButtons, cxControls, cxFilterControl, cxLookAndFeels, dxForms, cxClasses,
  dxLayoutContainer, dxLayoutControl, dxLayoutControlAdapters, dxLayoutLookAndFeels;

type
  TfmFilterControlDialog = class(TdxForm)
    btOpen: TcxButton;
    btSave: TcxButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    btApply: TcxButton;
    btCancel: TcxButton;
    btOk: TcxButton;
    lcMainGroup_Root: TdxLayoutGroup;
    lcMain: TdxLayoutControl;
    lgButtons: TdxLayoutGroup;
    libtOpen: TdxLayoutItem;
    libtSave: TdxLayoutItem;
    libtOk: TdxLayoutItem;
    libtCancel: TdxLayoutItem;
    libtApply: TdxLayoutItem;
    liFilterControl: TdxLayoutItem;
    dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList;
    dxLayoutSkinLookAndFeel: TdxLayoutSkinLookAndFeel;
    procedure acApplyExecute(Sender: TObject);
    procedure acOkExecute(Sender: TObject);
    procedure acOpenExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FOnAfterApply: TNotifyEvent;
    FOnBeforeApply: TNotifyEvent;
    FHasOpenedPopupWindow: Boolean;
    FFilterControl: TcxCustomFilterControl;
    procedure Initialize(const AInitialDir: string);
    procedure SetControlsEnabled(AEnabled: Boolean);
    procedure SetLookAndFeel(ALookAndFeel: TcxLookAndFeel);
    procedure SetTitle(const ATitle: string);
  protected
    procedure CreateParams(var Params: TCreateParams); override;

    procedure ApplyFilter; virtual;
    procedure DoAfterApply; virtual;
    procedure DoBeforeApply; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    property FilterControl: TcxCustomFilterControl read FFilterControl;
    property OnAfterApply: TNotifyEvent read FOnAfterApply write FOnAfterApply;
    property OnBeforeApply: TNotifyEvent read FOnBeforeApply write FOnBeforeApply;
  end;

function cxInternalExecuteFilterControlDialog(AFilterControlClass: TcxCustomFilterControlClass; ALink: TComponent; ALookAndFeel: TcxLookAndFeel; AOnApplyProc: TNotifyEvent = nil; AOnShowDialog:
    TNotifyEvent = nil; AColor: TColor = clDefault; const AInitialDir: string = ''; AFont: TFont = nil; AOnBeforeApply: TNotifyEvent = nil; AOnAfterApply: TNotifyEvent = nil; AControl: TControl =
    nil): Boolean;

function ExecuteFilterControlDialog(ALinkComponent: TComponent; ALookAndFeel: TcxLookAndFeel; AOnApplyProc: TNotifyEvent = nil; AOnShowDialog: TNotifyEvent = nil; AColor: TColor = clDefault; const
    AInitialDir: string = ''; AFont: TFont = nil; AOnBeforeApply: TNotifyEvent = nil; AOnAfterApply: TNotifyEvent = nil; AControl: TControl = nil): Boolean;

const
  cxFilterDialogPosition: TRect =
    (Left: -1; Top: -1;           // screen center
     Right: -1; Bottom: -1);      // BottomRight as TSize

implementation

{$R *.dfm}

uses
  cxFilterControlStrs, cxContainer;

function cxInternalExecuteFilterControlDialog(AFilterControlClass: TcxCustomFilterControlClass; ALink: TComponent; ALookAndFeel: TcxLookAndFeel; AOnApplyProc: TNotifyEvent = nil; AOnShowDialog:
    TNotifyEvent = nil; AColor: TColor = clDefault; const AInitialDir: string = ''; AFont: TFont = nil; AOnBeforeApply: TNotifyEvent = nil; AOnAfterApply: TNotifyEvent = nil; AControl: TControl = nil): Boolean;
var
  AForm: TfmFilterControlDialog;
  AIntf: IcxFilterControlDialog;
begin
  AForm := TfmFilterControlDialog.Create(Application);
  try
    AForm.OnBeforeApply := AOnBeforeApply;
    AForm.OnAfterApply := AOnAfterApply;
    if AControl <> nil then
      AForm.BiDiMode := AControl.BiDiMode
    else
      if ALink is TControl then
        AForm.BiDiMode := TControl(ALink).BiDiMode;
    AForm.FFilterControl := AFilterControlClass.Create(nil);
    AForm.SetLookAndFeel(ALookAndFeel);
    DialogApplyFont(AForm, AFont, ALink);

    if Supports(TObject(AForm.FFilterControl), IcxFilterControlDialog, AIntf) then
    try
      AIntf.SetDialogLinkComponent(ALink);
    finally
      AIntf := nil; //force to clear interface
    end;

    if AColor <> clDefault then
      AForm.FilterControl.Color := AColor;

    AForm.FilterControl.OnApplyFilter := AOnApplyProc;
    AForm.Initialize(AInitialDir);
    AForm.OnShow := AOnShowDialog;

    AForm.lcMain.HandleNeeded;
    AForm.ClientWidth := AForm.lcMain.Items.ViewInfo.MinWidth;
    AForm.Constraints.MinWidth := AForm.Width;

    Result := AForm.ShowModal = mrOk;
  finally
    AForm.FFilterControl.Free;
    AForm.Free;
  end;
end;

function ExecuteFilterControlDialog(ALinkComponent: TComponent; ALookAndFeel: TcxLookAndFeel; AOnApplyProc: TNotifyEvent = nil; AOnShowDialog: TNotifyEvent = nil; AColor: TColor = clDefault; const
    AInitialDir: string = ''; AFont: TFont = nil; AOnBeforeApply: TNotifyEvent = nil; AOnAfterApply: TNotifyEvent = nil; AControl: TControl = nil): Boolean;
begin
  Result := cxInternalExecuteFilterControlDialog(TcxFilterControl,
    ALinkComponent, ALookAndFeel, AOnApplyProc, AOnShowDialog, AColor,
    AInitialDir, AFont, AOnBeforeApply, AOnAfterApply, AControl);
end;

{ TfmFilterControlDialog }

constructor TfmFilterControlDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Position := poDesigned;
end;

procedure TfmFilterControlDialog.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
// rollback  CorrectDlgParams(Params);
  Params.Style := Params.Style or WS_POPUP; // for PopupEdit placed on DockPanel
end;

procedure TfmFilterControlDialog.ApplyFilter;
begin
  SetControlsEnabled(False);
  DoBeforeApply;
  try
    FilterControl.ApplyFilter;
  finally
    DoAfterApply;
    SetControlsEnabled(True);
  end;
end;

procedure TfmFilterControlDialog.DoAfterApply;
begin
  if Assigned(FOnAfterApply) then
    FOnAfterApply(Self);
end;

procedure TfmFilterControlDialog.DoBeforeApply;
begin
  if Assigned(FOnBeforeApply) then
    FOnBeforeApply(Self);
end;

procedure TfmFilterControlDialog.Initialize(const AInitialDir: string);
begin
  SetTitle(cxGetResourceString(@cxSFilterControlDialogNewFile));
  OpenDialog.InitialDir := AInitialDir;
  OpenDialog.Title := cxGetResourceString(@cxSFilterControlDialogOpenDialogCaption);
  OpenDialog.DefaultExt := cxGetResourceString(@cxSFilterControlDialogFileExt);
  OpenDialog.Filter := cxGetResourceString(@cxSFilterControlDialogFileFilter);
  SaveDialog.InitialDir := AInitialDir;
  SaveDialog.Title := cxGetResourceString(@cxSFilterControlDialogSaveDialogCaption);
  SaveDialog.DefaultExt := OpenDialog.DefaultExt;
  SaveDialog.Filter := OpenDialog.Filter;
  btSave.Caption := cxGetResourceString(@cxSFilterControlDialogActionSaveCaption);
  btSave.Hint := cxGetResourceString(@cxSFilterControlDialogActionSaveHint);
  btOpen.Caption := cxGetResourceString(@cxSFilterControlDialogActionOpenCaption);
  btOpen.Hint := cxGetResourceString(@cxSFilterControlDialogActionOpenHint);
  btApply.Caption := cxGetResourceString(@cxSFilterControlDialogActionApplyCaption);
  btOk.Caption := cxGetResourceString(@cxSFilterControlDialogActionOkCaption);
  btCancel.Caption := cxGetResourceString(@cxSFilterControlDialogActionCancelCaption);
  liFilterControl.Control := FilterControl;
  liFilterControl.Offsets.Left := -1;
  liFilterControl.Offsets.Right := -1;
  liFilterControl.Offsets.Top := -1;
  with cxFilterDialogPosition do
  begin
    if (Left = -1) or (Top = -1) then
      Position := poScreenCenter
    else
    begin
      Self.Left := Left;
      Self.Top := Top;
    end;
    if Right > 0 then Width := Right;
    if Bottom > 0 then Height := Bottom;
  end;
end;

procedure TfmFilterControlDialog.SetControlsEnabled(AEnabled: Boolean);
begin
  btOpen.Enabled := AEnabled;
  btSave.Enabled := AEnabled;
  btApply.Enabled := AEnabled;
  btCancel.Enabled := AEnabled;
  btOk.Enabled := AEnabled;
end;

procedure TfmFilterControlDialog.SetLookAndFeel(
  ALookAndFeel: TcxLookAndFeel);
begin
  if ALookAndFeel <> nil then
  begin
    btOk.LookAndFeel.MasterLookAndFeel := ALookAndFeel;
    btCancel.LookAndFeel.MasterLookAndFeel := ALookAndFeel;
    btApply.LookAndFeel.MasterLookAndFeel := ALookAndFeel;
    btOpen.LookAndFeel.MasterLookAndFeel := ALookAndFeel;
    btSave.LookAndFeel.MasterLookAndFeel := ALookAndFeel;
    FilterControl.LookAndFeel.MasterLookAndFeel := ALookAndFeel;
  end;
end;

procedure TfmFilterControlDialog.SetTitle(const ATitle: string);
begin
  SaveDialog.FileName := ATitle;
  Caption := Format('%s - [%s]', [cxGetResourceString(@cxSFilterControlDialogCaption), ATitle]);
end;

procedure TfmFilterControlDialog.acApplyExecute(Sender: TObject);
begin
  ApplyFilter;
end;

procedure TfmFilterControlDialog.acOkExecute(Sender: TObject);
begin
  ApplyFilter;
end;

procedure TfmFilterControlDialog.acOpenExecute(Sender: TObject);
begin
  OpenDialog.FileName := '';
  if OpenDialog.Execute then
  begin
    FilterControl.LoadFromFile(OpenDialog.FileName);
    SetTitle(OpenDialog.FileName);
  end;
end;

procedure TfmFilterControlDialog.acSaveExecute(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    FilterControl.SaveToFile(SaveDialog.FileName);
    SetTitle(SaveDialog.FileName);
  end;
end;

procedure TfmFilterControlDialog.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  cxFilterDialogPosition := Rect(Left, Top, Width, Height);
end;

procedure TfmFilterControlDialog.FormKeyPress(Sender: TObject;
  var Key: Char);
begin
  if (Key = #27) and not FHasOpenedPopupWindow then
    Close;
end;

procedure TfmFilterControlDialog.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  FHasOpenedPopupWindow := HasOpenedPopupWindow(ActiveControl);
end;

end.
