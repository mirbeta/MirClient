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

unit dxPSReportLinkDesignWindow;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, dxPSCore, StdCtrls, ExtCtrls,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxClasses, dxLayoutContainer, dxLayoutControl,
  dxLayoutLookAndFeels, Menus, dxLayoutControlAdapters, cxButtons;

type
  { TStandarddxReportLinkDesignWindow }

  TStandarddxReportLinkDesignWindow = class(TAbstractdxReportLinkDesignWindow)
    btnApply: TcxButton;
    btnCancel: TcxButton;
    btnFootnoteProperties: TcxButton;
    btnHelp: TcxButton;
    btnOK: TcxButton;
    btnRestoreDefaults: TcxButton;
    btnRestoreOriginal: TcxButton;
    btnTitleProperties: TcxButton;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    libtnApply: TdxLayoutItem;
    libtnCancel: TdxLayoutItem;
    libtnFootnoteProperties: TdxLayoutItem;
    libtnHelp: TdxLayoutItem;
    libtnOk: TdxLayoutItem;
    libtnRestoreDefaults: TdxLayoutItem;
    libtnRestoreOriginal: TdxLayoutItem;
    libtnTitleProperties: TdxLayoutItem;
    dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem;
    dxLayoutEmptySpaceItem2: TdxLayoutEmptySpaceItem;

    procedure ApplyClick(Sender: TObject); virtual;
    procedure FootnotesPropertiesClick(Sender: TObject); virtual;
    procedure RestoreDefaultsClick(Sender: TObject); virtual;
    procedure RestoreOriginalClick(Sender: TObject); virtual;
    procedure TitlePropertiesClick(Sender: TObject); virtual;
  strict private
    FAtLeastOneTimeApplied: Boolean;
    FUpdateControlsCount: Integer;

    procedure SetAtLeastOneTimeApplied(Value: Boolean);
    procedure RegroupStdButtons;
  protected
    procedure DoApply; virtual;
    procedure Initialize; override;
    procedure LoadStrings; override;
    procedure UpdateControlsState; override;

    procedure DoInitialize; virtual;
    procedure LoadGroupsIcons; virtual;
    function GetPreviewHost: TCustomPanel; virtual;
    procedure PaintPreview(ACanvas: TCanvas; R: TRect); virtual;
    procedure ScaleFactorChanged(M, D: Integer); override;
    procedure UpdatePreview; virtual;

    property AtLeastOneTimeApplied: Boolean read FAtLeastOneTimeApplied write SetAtLeastOneTimeApplied;
    property PreviewHost: TCustomPanel read GetPreviewHost;
    property UpdateControlsCount: Integer read FUpdateControlsCount;
  public
    procedure AfterConstruction; override;
    procedure BeginUpdateControls; virtual;
    procedure EndUpdateControls; virtual;
    function LockControlsUpdate: Boolean;
  end;

implementation

uses
  dxPSUtl, dxCoreClasses, dxCore, dxPSRes, dxThemeConsts, dxThemeManager, dxUxTheme, Themes;

{$R *.dfm}

type
  TBasedxReportLinkAccess = class(TBasedxReportLink);

{ TStandarddxReportLinkDesignWindow }

procedure TStandarddxReportLinkDesignWindow.AfterConstruction;
begin
  inherited AfterConstruction;
  LoadGroupsIcons;
end;

procedure TStandarddxReportLinkDesignWindow.FootnotesPropertiesClick(Sender: TObject);
begin
  if (ReportLink <> nil) and ReportLink.ShowFootnotesPropertiesDlg then
  begin
    if not ReportLink.AbortBuilding then
    begin
      AtLeastOneTimeApplied := True;
      Applyed := True;
    end;
    UpdateControlsState;
  end;
end;

procedure TStandarddxReportLinkDesignWindow.RestoreOriginalClick(Sender: TObject);
begin
  BeginUpdateControls;
  try
    if ReportLink <> nil then
      ReportLink.RestoreFromOriginal;
    DoInitialize;
  finally
    EndUpdateControls;
  end;
  Modified := True;
end;

procedure TStandarddxReportLinkDesignWindow.RestoreDefaultsClick(Sender: TObject);
begin
  BeginUpdateControls;
  try
    if ReportLink <> nil then
      ReportLink.RestoreDefaults;
    DoInitialize;
  finally
    EndUpdateControls;
  end;
  Modified := True;
end;

procedure TStandarddxReportLinkDesignWindow.TitlePropertiesClick(Sender: TObject);
begin
  if (ReportLink <> nil) and ReportLink.ShowTitlePropertiesDlg then
  begin
    if not ReportLink.AbortBuilding then
    begin
      AtLeastOneTimeApplied := True;
      Applyed := True;
    end;
    UpdateControlsState;
  end;
end;

procedure TStandarddxReportLinkDesignWindow.ApplyClick(Sender: TObject);
begin
  DoApply;
end;

procedure TStandarddxReportLinkDesignWindow.RegroupStdButtons;
begin
  CheckDialogFormHelpContext(Self, libtnHelp);

  libtnRestoreOriginal.Visible := TBasedxReportLinkAccess(ReportLink).IsDesigning;
  libtnRestoreDefaults.Visible := TBasedxReportLinkAccess(ReportLink).IsDesigning;
  libtnFootnoteProperties.Visible := libtnFootnoteProperties.Visible and
    ReportLink.CanChangeFootnotes and not TBasedxReportLinkAccess(ReportLink).IsDesigning;
  libtnTitleProperties.Visible := libtnTitleProperties.Visible and
    ReportLink.CanChangeTitle and not TBasedxReportLinkAccess(ReportLink).IsDesigning;
end;

procedure TStandarddxReportLinkDesignWindow.DoApply;
begin
  try
    TBasedxReportLinkAccess(ReportLink).DoApplyInDesigner;
  except
    Application.HandleException(Self);
    ModalResult := mrCancel;
    raise;
  end;
  if not ReportLink.AbortBuilding then
  begin
    AtLeastOneTimeApplied := True;
    Applyed := True;
  end;
  UpdateControlsState;
end;

procedure TStandarddxReportLinkDesignWindow.Initialize;
begin
  BeginUpdateControls;
  try
    inherited Initialize;
    RegroupStdButtons;
    DoInitialize;
  finally
    UpdateControlsState;
    EndUpdateControls;
  end;
end;

procedure TStandarddxReportLinkDesignWindow.LoadGroupsIcons;
begin
end;

procedure TStandarddxReportLinkDesignWindow.LoadStrings;
begin
  inherited LoadStrings;
  btnOK.Caption := cxGetResourceString(@sdxBtnOK);
  btnCancel.Caption := cxGetResourceString(@sdxBtnCancel);
  btnApply.Caption := cxGetResourceString(@sdxBtnApply);
  btnHelp.Caption := cxGetResourceString(@sdxBtnHelp);
  btnRestoreDefaults.Caption := cxGetResourceString(@sdxBtnRestoreDefaults);
  btnRestoreOriginal.Caption := cxGetResourceString(@sdxBtnRestoreOriginal);
  btnTitleProperties.Caption := cxGetResourceString(@sdxBtnTitleProperties);
  btnFootnoteProperties.Caption := cxGetResourceString(@sdxBtnFootnoteProperties);
end;

procedure TStandarddxReportLinkDesignWindow.UpdateControlsState;
begin
  if btnFootnoteProperties <> nil then
    btnFootnoteProperties.Enabled := not ReportLink.IsAggregated;
  if btnTitleProperties <> nil then
    btnTitleProperties.Enabled := not ReportLink.IsAggregated;
  if btnApply <> nil then
    btnApply.Enabled := CanApply;
  if btnRestoreOriginal <> nil then
    btnRestoreOriginal.Enabled := (ReportLink <> nil) and ReportLink.DataProviderPresent;
end;

procedure TStandarddxReportLinkDesignWindow.DoInitialize;
begin
end;

function TStandarddxReportLinkDesignWindow.GetPreviewHost: TCustomPanel;
begin
  Result := nil;
end;

procedure TStandarddxReportLinkDesignWindow.PaintPreview(ACanvas: TCanvas; R: TRect);
var
  Details: TThemedElementDetails;
begin
  if (PreviewHost <> nil) and cxIsVCLThemesEnabled then
  begin
    Details := cxStyleServices.GetElementDetails(ttBody);
    cxStyleServices.DrawElement(ACanvas.Handle, Details, PreviewHost.Parent.ClientRect);
    Exit;
  end;
  ACanvas.Brush.Color := clWindow;
  ACanvas.FillRect(R);
end;

procedure TStandarddxReportLinkDesignWindow.ScaleFactorChanged(M, D: Integer);
begin
  inherited;
  if ReportLink <> nil then
    UpdatePreview;
end;

procedure TStandarddxReportLinkDesignWindow.UpdatePreview;
begin
end;

procedure TStandarddxReportLinkDesignWindow.BeginUpdateControls;
begin
  Inc(FUpdateControlsCount);
end;

procedure TStandarddxReportLinkDesignWindow.EndUpdateControls;
begin
  if FUpdateControlsCount > 0 then
  begin
    Dec(FUpdateControlsCount);
    if FUpdateControlsCount = 0 then
      UpdatePreview;
  end;
end;

function TStandarddxReportLinkDesignWindow.LockControlsUpdate: Boolean;
begin
  Result := FUpdateControlsCount <> 0;
end;

procedure TStandarddxReportLinkDesignWindow.SetAtLeastOneTimeApplied(Value: Boolean);
begin
  if FAtLeastOneTimeApplied <> Value then
  begin
    FAtLeastOneTimeApplied := Value;
    if FAtLeastOneTimeApplied then
      btnCancel.Caption := cxGetResourceString(@sdxBtnClose)
    else
      btnCancel.Caption := cxGetResourceString(@sdxBtnCancel);
  end;
end;

end.
