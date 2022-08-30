{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid                                       }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL            }
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

unit cxGridViewLayoutEditor;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, cxStyles, cxGridLevel,
  cxControls, cxGrid, cxGridCustomView, cxGridCustomPopupMenu, cxGridPopupMenu, Menus, cxLookAndFeelPainters,
  cxContainer, cxEdit, cxCheckBox, cxButtons, cxGraphics, cxLookAndFeels, dxLayoutControlAdapters,
  dxLayoutcxEditAdapters, dxLayoutContainer, dxLayoutLookAndFeels, dxLayoutControl, cxClasses, dxForms;

type
  TcxGridViewLayoutEditor = class(TdxForm)
    btnCancel: TcxButton;
    btnLayoutCustomization: TcxButton;
    btnOK: TcxButton;
    chbSaveData: TcxCheckBox;
    chbSaveLayout: TcxCheckBox;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    liClient: TdxLayoutItem;
    liOK: TdxLayoutItem;
    liSaveData: TdxLayoutItem;
    liSaveLayout: TdxLayoutItem;
    pmGrid: TcxGridPopupMenu;
    pnlLayoutCustomization: TdxLayoutItem;
    btnConditionalFormatting: TcxButton;
    liConditionalFormatting: TdxLayoutItem;

    procedure btnLayoutCustomizationClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnConditionalFormattingClick(Sender: TObject);
  private
    FOriginalView: TcxCustomGridView;
    FView: TcxCustomGridView;

    function GetDataControllerSupport: IcxCustomGridDataController;
    function GetViewSupport: IcxGridViewLayoutEditorSupport;
  public
    constructor Create(AView: TcxCustomGridView); reintroduce; virtual;
    procedure Load;
    procedure Save;
    property DataControllerSupport: IcxCustomGridDataController read GetDataControllerSupport;
    property ViewSupport: IcxGridViewLayoutEditorSupport read GetViewSupport;
  end;

function ShowGridViewEditor(AView: TcxCustomGridView): Boolean;

implementation

{$R *.dfm}

uses
  cxData, cxDataControllerConditionalFormatting;

const
  CloseButtonCaption = 'Close';

type
  TcxCustomGridViewAccess = class(TcxCustomGridView);

function ShowGridViewEditor(AView: TcxCustomGridView): Boolean;
var
  AForm: TcxGridViewLayoutEditor;
begin
  Result := TcxCustomGridViewAccess(AView).ShowGridViewEditor;
  if Result then
    Exit;
  AForm := TcxGridViewLayoutEditor.Create(AView);
  try
    Result := AForm.ShowModal = mrOK;
    if Result then AForm.Save;
  finally
    AForm.Free;
  end;
end;

{ TcxGridViewLayoutEditor }

constructor TcxGridViewLayoutEditor.Create(AView: TcxCustomGridView);
var
  AGrid: TcxGrid;
begin
  inherited Create(nil);
  FOriginalView := AView;

  AGrid := TcxGrid.Create(Self);
  liClient.Control := AGrid;
  if FOriginalView.LookAndFeel <> nil then
    AGrid.LookAndFeel := FOriginalView.LookAndFeel;
  FView := AGrid.CreateView(TcxCustomGridViewClass(FOriginalView.ClassType));
  AGrid.Levels.Add.GridView := FView;
  ActiveControl := AGrid;

  Load;
end;

function TcxGridViewLayoutEditor.GetDataControllerSupport: IcxCustomGridDataController;
begin
  Result := FOriginalView.DataController as IcxCustomGridDataController;
end;

function TcxGridViewLayoutEditor.GetViewSupport: IcxGridViewLayoutEditorSupport;
begin
  Result := FOriginalView as IcxGridViewLayoutEditorSupport;
end;

procedure TcxGridViewLayoutEditor.Load;
begin
  FView.Assign(FOriginalView);
  (FView.DataController as IcxCustomGridDataController).AssignData(FOriginalView.DataController);
  ViewSupport.BeforeEditLayout(FView);
end;

procedure TcxGridViewLayoutEditor.Save;
begin
  if ViewSupport.IsLayoutChangeable and chbSaveLayout.Checked then
    ViewSupport.DoAssignLayout(FView);
  if DataControllerSupport.IsDataChangeable and chbSaveData.Checked then
    DataControllerSupport.AssignData(FView.DataController);
end;

procedure TcxGridViewLayoutEditor.btnConditionalFormattingClick(
  Sender: TObject);
var
  AIntf: IcxDataControllerConditionalFormattingProviderOwner;
begin
  AIntf := FView as IcxDataControllerConditionalFormattingProviderOwner;
  if (AIntf <> nil) and (AIntf.GetConditionalFormattingProvider <> nil) then
    AIntf.GetConditionalFormattingProvider.ConditionalFormatting.ShowRulesManagerDialog;
end;

procedure TcxGridViewLayoutEditor.btnLayoutCustomizationClick(Sender: TObject);
begin
  (FView as IcxGridViewLayoutEditorSupport).RunLayoutCustomizationForm;
end;

procedure TcxGridViewLayoutEditor.FormShow(Sender: TObject);
var
  AIntf: IcxDataControllerConditionalFormattingProviderOwner;
begin
  Caption := Caption + ' - ' + FOriginalView.Name;
  pnlLayoutCustomization.Visible := ViewSupport.HasLayoutCustomizationForm;
  btnLayoutCustomization.Caption := ViewSupport.GetLayoutCustomizationFormButtonCaption;
  liSaveLayout.Visible := ViewSupport.IsLayoutChangeable;
  liSaveData.Visible := DataControllerSupport.IsDataChangeable;
  liOK.Visible := ViewSupport.IsLayoutChangeable or DataControllerSupport.IsDataChangeable;
  liConditionalFormatting.Visible := Supports(FView, IcxDataControllerConditionalFormattingProviderOwner, AIntf) and
    (AIntf.GetConditionalFormattingProvider <> nil) and
    AIntf.GetConditionalFormattingProvider.ConditionalFormatting.CanShowRulesManagerDialog;
  if not liOK.Visible then
    btnCancel.Caption := CloseButtonCaption;
  pmGrid.Grid := TcxGrid(FView.Control);
end;

end.
