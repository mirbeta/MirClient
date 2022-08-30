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

unit cxSchedulerTaskDependencyEditor;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus, StdCtrls,
  dxCore, cxClasses, cxSchedulerStorage, cxGraphics, cxTextEdit, cxMaskEdit,
  cxDropDownEdit, cxControls, cxContainer, cxEdit, cxLabel,
  cxLookAndFeelPainters, cxButtons, cxGroupBox, cxLookAndFeels, dxLayoutcxEditAdapters, dxLayoutControlAdapters,
  dxLayoutLookAndFeels, dxLayoutContainer, dxLayoutControl, dxForms;

type
  TfmSchedulerTaskDependencyEditor = class(TdxForm)
    cbTypeRelation: TcxComboBox;
    btnCancel: TcxButton;
    btnOk: TcxButton;
    btnDelete: TcxButton;
    lcMainGroup_Root: TdxLayoutGroup;
    lcMain: TdxLayoutControl;
    lbType: TdxLayoutItem;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    lbToName: TcxLabel;
    lbTo: TdxLayoutItem;
    lbFrom: TdxLayoutItem;
    lbFromName: TcxLabel;
    procedure cbTypeRelationPropertiesChange(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FIsDelete: Boolean;
    FEventFrom: TcxSchedulerEvent;
    FEventTo: TcxSchedulerEvent;
    FIsModified: Boolean;
    FLinkRelation: TcxSchedulerEventRelation;
    procedure CheckButtonState;
    procedure SetLinkRelation(AValue: TcxSchedulerEventRelation);
  protected
    function GetEventStr(const AEvent: TcxSchedulerEvent): string; virtual;
    procedure InitControls; virtual;
    procedure SetCaptions;
  public
    function ShowModal: Integer; override;

    property IsDelete: Boolean read FIsDelete;
    property EventFrom: TcxSchedulerEvent read FEventFrom write FEventFrom;
    property EventTo: TcxSchedulerEvent read FEventTo write FEventTo;
    property IsModified: Boolean read FIsModified;
    property LinkRelation: TcxSchedulerEventRelation read FLinkRelation write SetLinkRelation;
  end;

TcxSchedulerTaskDependencyEditor = TfmSchedulerTaskDependencyEditor;
TcxSchedulerTaskDependencyEditorClass = class of TcxSchedulerTaskDependencyEditor;

implementation

uses
  cxSchedulerStrs;

{$R *.dfm}

function TfmSchedulerTaskDependencyEditor.ShowModal: Integer;
begin
  FIsModified := False;
  InitControls;
  FIsDelete := False;
  Result := inherited ShowModal;
end;

function TfmSchedulerTaskDependencyEditor.GetEventStr(const AEvent: TcxSchedulerEvent): string;
begin
  Result := AEvent.Caption;
end;

procedure TfmSchedulerTaskDependencyEditor.InitControls;
begin
  with cbTypeRelation.ActiveProperties.Items do
  begin
    Clear;
    Add(cxGetResourceString(@scxFinishToStartLong));
    Add(cxGetResourceString(@scxStartToStartLong));
    Add(cxGetResourceString(@scxFinishToFinishLong));
    Add(cxGetResourceString(@scxStartToFinishLong));
  end;
  cbTypeRelation.ItemIndex := Integer(LinkRelation);
  SetCaptions;
  FIsModified := False;
  CheckButtonState;
end;

procedure TfmSchedulerTaskDependencyEditor.SetCaptions;
begin
  Caption := cxGetResourceString(@scxTaskDependencyEditorCaption);
  btnOk.Caption := cxGetResourceString(@scxOk);
  btnCancel.Caption := cxGetResourceString(@scxCancel);
  btnDelete.Caption := cxGetResourceString(@scxDelete);
  lbFrom.Caption := cxGetResourceString(@scxFrom);
  lbTo.Caption := cxGetResourceString(@scxTo);
  lbType.Caption := cxGetResourceString(@scxType);
  lbFromName.Caption := cxGetStringAdjustedToWidth(lbFromName.Style.Font, GetEventStr(FEventFrom), lbFromName.ClientWidth);
  lbToName.Caption := cxGetStringAdjustedToWidth(lbToName.Style.Font, GetEventStr(FEventTo), lbToName.ClientWidth);
end;

procedure TfmSchedulerTaskDependencyEditor.CheckButtonState;
begin
  btnOk.Enabled := IsModified;
end;

procedure TfmSchedulerTaskDependencyEditor.SetLinkRelation(AValue: TcxSchedulerEventRelation);
begin
  if AValue <> FLinkRelation then
  begin
    FLinkRelation := AValue;
    FIsModified := True;
    CheckButtonState;
  end;
end;

procedure TfmSchedulerTaskDependencyEditor.cbTypeRelationPropertiesChange(Sender: TObject);
begin
  LinkRelation := TcxSchedulerEventRelation(cbTypeRelation.ItemIndex);
  CheckButtonState;
end;

procedure TfmSchedulerTaskDependencyEditor.btnDeleteClick(Sender: TObject);
begin
  FIsDelete := True;
end;

procedure TfmSchedulerTaskDependencyEditor.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    ModalResult := mrCancel;
    Key := 0;
  end;
end;

end.
