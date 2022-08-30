{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid                                       }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{ The entire contents of this file is protected by U.S. and          }
{ International Copyright Laws. Unauthorized reproduction,           }
{ reverse-engineering, and distribution of all or any portion of     }
{ the code contained in this file is strictly prohibited and may     }
{ result in severe civil and criminal penalties and will be          }
{ prosecuted to the maximum extent possible under the law.           }
{                                                                    }
{ RESTRICTIONS                                                       }
{                                                                    }
{ THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES              }
{ (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE       }
{ SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS      }
{ LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL              }
{ ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.   }
{                                                                    }
{ THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED         }
{ FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE           }
{ COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE          }
{ AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT     }
{ AND PERMISSION FROM DEVELOPER EXPRESS INC.                         }
{                                                                    }
{ CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON          }
{ ADDITIONAL RESTRICTIONS.                                           }
{                                                                    }
{ ******************************************************************** }

unit cxGridStyleSheetsPreview;

{$I cxVer.inc}

interface

uses
  Windows, Messages, Classes, Controls, SysUtils, cxStyles, cxGridCustomView,
  cxGridLevel, cxGrid;

implementation

uses
  cxStyleSheetEditor, cxCustomData, cxGridTableView, cxDataStorage,
  cxGridCustomTableView, cxGridBandedTableView, cxGridCardView, cxGridLayoutView;

type
  { TcxCustomViewStyleSheetEditorPreview }

  TcxCustomViewStyleSheetEditorPreview = class(TcxStyleSheetEditorPreview)
  private
    FGrid: TcxGrid;
  protected
    RootLevel: TcxGridLevel;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Control: TWinControl; override;
  end;

  { TcxCustomTableViewStyleSheetEditorPreview }

  TcxCustomTableViewStyleSheetEditorPreview = class(TcxCustomViewStyleSheetEditorPreview)
  protected
    procedure CreateData; virtual;
    function TableView: TcxGridTableView; virtual; abstract;
    procedure SetupView; virtual;
  end;

  { TcxTableViewStyleSheetEditorPreview }

  TcxTableViewStyleSheetEditorPreview = class(TcxCustomTableViewStyleSheetEditorPreview)
  private
    FTableView: TcxGridTableView;
  protected
    function TableView: TcxGridTableView; override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetStyleSheetClass: TcxCustomStyleSheetClass; override;
    procedure SetStyleSheet(AStyleSheet: TcxCustomStyleSheet); override;
  end;

  { TcxBandedTableViewStyleSheetEditorPreview }

  TcxBandedTableViewStyleSheetEditorPreview = class(TcxCustomTableViewStyleSheetEditorPreview)
  private
    FBandedTableView: TcxGridBandedTableView;
  protected
    function TableView: TcxGridTableView; override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetStyleSheetClass: TcxCustomStyleSheetClass; override;
    procedure SetStyleSheet(AStyleSheet: TcxCustomStyleSheet); override;
  end;

  { TcxCardViewStyleSheetEditorPreview }

  TcxCardViewStyleSheetEditorPreview = class(TcxCustomViewStyleSheetEditorPreview)
  protected
    FCardView: TcxGridCardView;
    procedure CreateData;
    procedure SetupView;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetStyleSheetClass: TcxCustomStyleSheetClass; override;
    procedure SetStyleSheet(AStyleSheet: TcxCustomStyleSheet); override;
  end;

  { TcxLayoutViewStyleSheetEditorPreview }

  TcxLayoutViewStyleSheetEditorPreview = class(TcxCustomViewStyleSheetEditorPreview)
  protected
    FLayoutView: TcxGridLayoutView;
    procedure CreateData;
    procedure SetupView;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetStyleSheetClass: TcxCustomStyleSheetClass; override;
    procedure SetStyleSheet(AStyleSheet: TcxCustomStyleSheet); override;
  end;

{ TcxCustomViewStyleSheetEditorPreview }

constructor TcxCustomViewStyleSheetEditorPreview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGrid := TcxGrid.Create(AOwner);
  RootLevel := FGrid.Levels.Add;
end;

destructor TcxCustomViewStyleSheetEditorPreview.Destroy;
begin
  FGrid.Free;
  inherited Destroy;
end;

function TcxCustomViewStyleSheetEditorPreview.Control: TWinControl;
begin
  Result := FGrid;
end;

{ TcxCustomTableViewStyleSheetEditorPreview }

procedure TcxCustomTableViewStyleSheetEditorPreview.CreateData;
var
  I: Integer;
begin
  TableView.CreateColumn;
  TableView.CreateColumn;
  TableView.CreateColumn;
  TableView.CreateColumn;
  TableView.Columns[0].Caption := 'Text';
  TableView.Columns[1].Caption := 'Currency';
  TableView.Columns[2].Caption := 'Date';
  TableView.Columns[0].DataBinding.ValueTypeClass := TcxStringValueType;
  TableView.Columns[1].DataBinding.ValueTypeClass := TcxCurrencyValueType;
  TableView.Columns[2].DataBinding.ValueTypeClass := TcxDateTimeValueType;
  TableView.Columns[3].DataBinding.ValueTypeClass := TcxStringValueType;
  TableView.Columns[2].GroupIndex := 0;
  TableView.Columns[3].Visible := False;
  TableView.Preview.Visible := True;
  TableView.Preview.Column := TableView.Columns[3];

  with TableView.DataController as TcxGridDataController do
  begin
    BeginUpdate;
    RecordCount := 3;
    try
      for I := 0 to 2 do
      begin
        Values[I, 0] := 'ABC';
        Values[I, 1] := (I + 1) * 100;
        Values[I, 2] := Date - I;
        Values[I, 3] := 'Check the preview style';
      end;
    finally
      EndUpdate;
    end;
  end;
  TableView.DataController.Groups.FullExpand;
  TableView.DataController.Summary.FooterSummaryItems.Add;
  TableView.DataController.Summary.FooterSummaryItems.Items[0].Kind := skSum;
  TableView.DataController.Summary.FooterSummaryItems.Items[0].ItemLink := TableView.Columns[1];
end;

procedure TcxCustomTableViewStyleSheetEditorPreview.SetupView;
begin
  TableView.OptionsView.ColumnAutoWidth := True;
  TableView.OptionsView.Footer := True;
  TableView.OptionsView.Indicator := True;
  TableView.OptionsData.Deleting := False;
  TableView.OptionsData.Editing := False;
  TableView.OptionsData.Appending := False;
  TableView.OptionsData.Inserting := False;
  TableView.OptionsBehavior.IncSearch := True;
end;

{ TcxTableViewStyleSheetEditorPreview }

constructor TcxTableViewStyleSheetEditorPreview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTableView := TcxGridTableView(FGrid.CreateView(TcxGridTableView));
  RootLevel.GridView := FTableView;
  SetupView;
  CreateData;
end;

function TcxTableViewStyleSheetEditorPreview.TableView: TcxGridTableView;
begin
  Result := FTableView;
end;

class function TcxTableViewStyleSheetEditorPreview.GetStyleSheetClass: TcxCustomStyleSheetClass;
begin
  Result := TcxGridTableViewStyleSheet;
end;

procedure TcxTableViewStyleSheetEditorPreview.SetStyleSheet(AStyleSheet: TcxCustomStyleSheet);
begin
  FTableView.Styles.StyleSheet := AStyleSheet;
end;

{ TcxBandedTableViewStyleSheetEditorPreview }

constructor TcxBandedTableViewStyleSheetEditorPreview.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited Create(AOwner);
  FBandedTableView := TcxGridBandedTableView(FGrid.CreateView(TcxGridBandedTableView));
  RootLevel.GridView := FBandedTableView;
  SetupView;
  CreateData;
  FBandedTableView.OptionsView.BandHeaders := True;
  FBandedTableView.Bands.Add;
  FBandedTableView.Bands[0].Caption := 'Band';
  for I := 0 to FBandedTableView.ColumnCount - 1 do
    FBandedTableView.Columns[I].Position.BandIndex := 0;
end;

function TcxBandedTableViewStyleSheetEditorPreview.TableView: TcxGridTableView;
begin
  Result := FBandedTableView;
end;

class function TcxBandedTableViewStyleSheetEditorPreview.GetStyleSheetClass: TcxCustomStyleSheetClass;
begin
  Result := TcxGridBandedTableViewStyleSheet;
end;

procedure TcxBandedTableViewStyleSheetEditorPreview.SetStyleSheet(AStyleSheet: TcxCustomStyleSheet);
begin
  FBandedTableView.Styles.StyleSheet := AStyleSheet;
end;

{ TcxCardViewStyleSheetEditorPreview }

constructor TcxCardViewStyleSheetEditorPreview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCardView := TcxGridCardView(FGrid.CreateView(TcxGridCardView));
  RootLevel.GridView := FCardView;
  SetupView;
  CreateData;
end;

class function TcxCardViewStyleSheetEditorPreview.GetStyleSheetClass: TcxCustomStyleSheetClass;
begin
  Result := TcxGridCardViewStyleSheet;
end;

procedure TcxCardViewStyleSheetEditorPreview.CreateData;
var
  I: Integer;
begin
  FCardView.CreateRow;
  FCardView.CreateRow;
  FCardView.CreateRow;
  FCardView.CreateRow;
  FCardView.Rows[0].Caption := 'Caption';
  FCardView.Rows[0].Kind := rkCaption;
  FCardView.Rows[1].Caption := 'Text';
  FCardView.Rows[2].Caption := 'Currency';
  FCardView.Rows[3].Caption := 'Date';
  FCardView.Rows[0].DataBinding.ValueTypeClass := TcxStringValueType;
  FCardView.Rows[1].DataBinding.ValueTypeClass := TcxStringValueType;
  FCardView.Rows[2].DataBinding.ValueTypeClass := TcxCurrencyValueType;
  FCardView.Rows[3].DataBinding.ValueTypeClass := TcxDateTimeValueType;

  with FCardView.DataController as TcxGridDataController do
  begin
    BeginUpdate;
    RecordCount := 3;
    try
      for I := 0 to 2 do
      begin
        Values[I, 0] := 'Caption';
        Values[I, 1] := 'ABC';
        Values[I, 2] := (I + 1) * 100;
        Values[I, 3] := Date - I;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TcxCardViewStyleSheetEditorPreview.SetupView;
begin
  FCardView.OptionsData.Deleting := False;
  FCardView.OptionsData.Editing := False;
  FCardView.OptionsData.Appending := False;
  FCardView.OptionsData.Inserting := False;
  FCardView.OptionsBehavior.IncSearch := True;
end;

procedure TcxCardViewStyleSheetEditorPreview.SetStyleSheet(AStyleSheet: TcxCustomStyleSheet);
begin
  FCardView.Styles.StyleSheet := AStyleSheet;
end;

{ TcxLayoutViewStyleSheetEditorPreview }

constructor TcxLayoutViewStyleSheetEditorPreview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLayoutView := TcxGridLayoutView(FGrid.CreateView(TcxGridLayoutView));
  RootLevel.GridView := FLayoutView;
  SetupView;
  CreateData;
end;

procedure TcxLayoutViewStyleSheetEditorPreview.CreateData;
var
  I: Integer;
begin
  FLayoutView.CreateItem;
  FLayoutView.CreateItem;
  FLayoutView.CreateItem;
  FLayoutView.CreateItem;
  FLayoutView.Items[0].Caption := 'Caption';
  FLayoutView.Items[1].Caption := 'Text';
  FLayoutView.Items[2].Caption := 'Currency';
  FLayoutView.Items[3].Caption := 'Date';
  FLayoutView.Items[0].DataBinding.ValueTypeClass := TcxStringValueType;
  FLayoutView.Items[1].DataBinding.ValueTypeClass := TcxStringValueType;
  FLayoutView.Items[2].DataBinding.ValueTypeClass := TcxCurrencyValueType;
  FLayoutView.Items[3].DataBinding.ValueTypeClass := TcxDateTimeValueType;

  with FLayoutView.DataController as TcxGridDataController do
  begin
    BeginUpdate;
    RecordCount := 3;
    try
      for I := 0 to 2 do
      begin
        Values[I, 0] := 'Caption';
        Values[I, 1] := 'ABC';
        Values[I, 2] := (I + 1) * 100;
        Values[I, 3] := Date - I;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

class function TcxLayoutViewStyleSheetEditorPreview.GetStyleSheetClass: TcxCustomStyleSheetClass;
begin
  Result := TcxGridLayoutViewStyleSheet;
end;

procedure TcxLayoutViewStyleSheetEditorPreview.SetStyleSheet(
  AStyleSheet: TcxCustomStyleSheet);
begin
  FLayoutView.Styles.StyleSheet := AStyleSheet;
end;

procedure TcxLayoutViewStyleSheetEditorPreview.SetupView;
begin
  FLayoutView.OptionsData.Deleting := False;
  FLayoutView.OptionsData.Editing := False;
  FLayoutView.OptionsData.Appending := False;
  FLayoutView.OptionsData.Inserting := False;
  FLayoutView.OptionsBehavior.IncSearch := True;
end;

initialization
  RegisterStyleSheetEditorPreview(TcxTableViewStyleSheetEditorPreview);
  RegisterStyleSheetEditorPreview(TcxBandedTableViewStyleSheetEditorPreview);
  RegisterStyleSheetEditorPreview(TcxCardViewStyleSheetEditorPreview);

finalization
  UnregisterStyleSheetEditorPreview(TcxCardViewStyleSheetEditorPreview);
  UnregisterStyleSheetEditorPreview(TcxBandedTableViewStyleSheetEditorPreview);
  UnregisterStyleSheetEditorPreview(TcxTableViewStyleSheetEditorPreview);

end.
