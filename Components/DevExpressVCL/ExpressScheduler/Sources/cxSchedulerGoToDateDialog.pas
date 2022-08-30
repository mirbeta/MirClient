{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressScheduler                                         }
{                                                                    }
{           Copyright (c) 2003-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSCHEDULER AND ALL ACCOMPANYING }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
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

unit cxSchedulerGoToDateDialog;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls, Menus, StdCtrls,
  dxCore, cxLookAndFeelPainters, cxButtons, cxDropDownEdit,
  cxTextEdit, cxMaskEdit, cxCalendar, cxControls, cxContainer, cxEdit,
  cxGroupBox, cxSchedulerCustomControls, cxGraphics, cxLookAndFeels,
  cxLabel, cxDateUtils, dxLayoutcxEditAdapters, dxLayoutControlAdapters, dxLayoutContainer, cxClasses, dxLayoutControl,
  dxLayoutLookAndFeels, dxForms;

type
  TfmGoToDateForm = class(TdxForm)
    btnCancel: TcxButton;
    btnOk: TcxButton;
    cbShowIn: TcxComboBox;
    deDate: TcxDateEdit;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    lbDate: TdxLayoutItem;
    lbShowIn: TdxLayoutItem;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
  protected
    procedure FillCombo(AScheduler: TcxCustomScheduler);
    function GetFormColor: TColor;
    procedure SetCaptions;
  public
    constructor CreateEx(AScheduler: TcxCustomScheduler; ADate: TDateTime); virtual;
    function GetViewMode: TcxSchedulerViewMode;
    function ShowModal: Integer; override;
  end;

implementation

uses
  cxSchedulerStrs, cxScheduler, cxSchedulerDialogs;

{$R *.dfm}

{ TfmGoToDateForm }

constructor TfmGoToDateForm.CreateEx(AScheduler: TcxCustomScheduler; ADate: TDateTime);
begin
  Create(Application);
  if ADate = NullDate then ADate := Date;
  deDate.Date := ADate;
  SetCaptions;
  FillCombo(AScheduler);
end;

function TfmGoToDateForm.GetViewMode: TcxSchedulerViewMode;
begin
  with cbShowIn do
  begin
    if ItemIndex < 0 then
      Result := vmDay
    else
      Result := TcxSchedulerViewMode(Properties.Items.Objects[ItemIndex]);
  end;
end;

function TfmGoToDateForm.ShowModal: Integer;
begin
  if UseSchedulerColorInDialogs then
    Color := GetFormColor;
  cxDialogsMetricsStore.InitDialog(Self);
  Result := inherited ShowModal;
  cxDialogsMetricsStore.StoreMetrics(Self);
end;

procedure TfmGoToDateForm.FillCombo(AScheduler: TcxCustomScheduler);
var
  AItemIndex: Integer;
begin
  AItemIndex := 0;
  with cbShowIn, Properties.Items do
  begin
    BeginUpdate;
    try
      if AScheduler is TcxScheduler then
        with TcxScheduler(AScheduler) do
        begin
          if ViewDay.CanShow then
            AddObject(cxGetResourceString(@scxDayCalendar), Pointer(0));
          if ViewWeek.CanShow then
          begin
            AddObject(cxGetResourceString(@scxWeekCalendar), Pointer(1));
            if ViewWeek.Active then AItemIndex := Count - 1;
          end;
          if ViewWeeks.CanShow then
          begin
            AddObject(cxGetResourceString(@scxMonthCalendar), Pointer(2));
            if ViewWeeks.Active then AItemIndex := Count - 1;
          end;
          if ViewDay.CanShow then
            AddObject(cxGetResourceString(@scxWorkWeekCalendar), Pointer(3));
          if ViewAgenda.CanShow then
          begin
            AddObject(cxGetResourceString(@scxAgendaCalendar), Pointer(4));
            if ViewAgenda.Active then AItemIndex := Count - 1;
          end;
        end
      else
      begin
        AddObject(cxGetResourceString(@scxDayCalendar), Pointer(0));
        AddObject(cxGetResourceString(@scxWeekCalendar), Pointer(1));
        AddObject(cxGetResourceString(@scxMonthCalendar), Pointer(2));
        AddObject(cxGetResourceString(@scxWorkWeekCalendar), Pointer(3));
        AddObject(cxGetResourceString(@scxAgendaCalendar), Pointer(4));
      end;
    finally
      EndUpdate;
      if Count > 0 then ItemIndex := AItemIndex else Enabled := False;
    end;
  end;
end;

function TfmGoToDateForm.GetFormColor: TColor;
begin
  Result := deDate.Style.LookAndFeel.Painter.DefaultSchedulerControlColor;
end;

procedure TfmGoToDateForm.SetCaptions;
begin
  Caption := cxGetResourceString(@scxGoToDateDialogCaption);
  // time
  lbDate.Caption := cxGetResourceString(@scxDate);
  lbShowIn.Caption := cxGetResourceString(@scxShowIn);
  // buttons
  btnOk.Caption := cxGetResourceString(@scxOk);
  btnCancel.Caption := cxGetResourceString(@scxCancel);
end;

end.
