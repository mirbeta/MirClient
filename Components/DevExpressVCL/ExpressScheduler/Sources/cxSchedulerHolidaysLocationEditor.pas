{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressDataController                                    }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSDATACONTROLLER AND ALL         }
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

unit cxSchedulerHolidaysLocationEditor;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus,
  dxCore, cxLookAndFeelPainters, StdCtrls, cxButtons, cxTextEdit,
  cxControls, cxContainer, cxEdit, cxLabel, cxSchedulerHolidays, cxLookAndFeels,
  cxGraphics, dxLayoutcxEditAdapters, dxLayoutControlAdapters, dxLayoutContainer, dxLayoutLookAndFeels, cxClasses,
  dxLayoutControl, dxForms;

type
  TfmHolidaysLocationEditor = class(TdxForm)
    btnOK: TcxButton;
    btnCancel: TcxButton;
    teName: TcxTextEdit;
    lcMainGroup_Root: TdxLayoutGroup;
    lcMain: TdxLayoutControl;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    lbName: TdxLayoutItem;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    procedure FormCreate(Sender: TObject);
    procedure teLocationPropertiesChange(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    FLocation: TcxSchedulerHolidaysLocation;
    FModify: Boolean;
    procedure SetLocation(const AValue: TcxSchedulerHolidaysLocation);
  protected
    procedure CheckButtonsState;
    procedure InitControls; virtual;
    function Save: Boolean; virtual;
    procedure SetCaptions;
  public
    function ShowModal: Integer; override;

    property Location: TcxSchedulerHolidaysLocation read FLocation write SetLocation;
    property Modify: Boolean read FModify;
  end;

implementation

uses
  cxSchedulerStrs, cxSchedulerDialogs, cxSchedulerUtils;

{$R *.dfm}

function TfmHolidaysLocationEditor.ShowModal: Integer;
begin
  cxDialogsMetricsStore.InitDialog(Self);
  InitControls;
  CheckButtonsState;
  Result := inherited ShowModal;
  cxDialogsMetricsStore.StoreMetrics(Self);
end;

procedure TfmHolidaysLocationEditor.CheckButtonsState;
begin
  btnOK.Enabled := FModify and (Length(teName.Text) > 0) and
    (FLocation.Locations.GetLocationByName(teName.Text) = nil);
end;

procedure TfmHolidaysLocationEditor.FormCreate(Sender: TObject);
begin
  FModify := False;
end;

procedure TfmHolidaysLocationEditor.InitControls;
begin
  if UseSchedulerColorInDialogs then
    Color := btnCancel.LookAndFeel.Painter.DefaultSchedulerControlColor;
  SetCaptions;
  teName.Text := FLocation.Name;
  FModify := False;
  CheckButtonsState;
end;

function TfmHolidaysLocationEditor.Save: Boolean;
begin
  Result := False;
  if not FModify then
    Exit;
  Result := True;
  FLocation.Name := teName.Text;
  FModify := False;
  CheckButtonsState;
end;

procedure TfmHolidaysLocationEditor.SetCaptions;
begin
  Caption := cxGetResourceString(@scxHolidaysLocationEditorCaption);
  btnCancel.Caption := cxGetResourceString(@scxCancel);
  btnOk.Caption := cxGetResourceString(@scxOk);
  lbName.Caption := cxGetResourceString(@scxLocationName);
end;

procedure TfmHolidaysLocationEditor.SetLocation(const AValue: TcxSchedulerHolidaysLocation);
begin
  FLocation := AValue;
end;

procedure TfmHolidaysLocationEditor.teLocationPropertiesChange(
  Sender: TObject);
begin
  FModify := True;
  CheckButtonsState;
end;

procedure TfmHolidaysLocationEditor.btnOKClick(Sender: TObject);
begin
  Save;
end;

end.
