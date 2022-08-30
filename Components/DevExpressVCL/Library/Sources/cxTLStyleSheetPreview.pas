{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumTreeList                                   }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMTREELIST AND ALL        }
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

unit cxTLStyleSheetPreview;

{$I cxVer.inc}

interface

uses
  Windows, Messages,
  Classes, Controls, SysUtils, cxStyles, cxTL, cxCustomData;

resourcestring
  cxTextDescription    = 'ABC';
  cxPreviewDescription = 'Check the preview style';

implementation

uses
  cxStyleSheetEditor, cxClasses;

type
  TcxTreeListStyleSheetEditorPreview = class(TcxStyleSheetEditorPreview)
  private
    FTreeList: TcxTreeList;
  protected
    procedure CreateData;
    procedure CreateColumns;
    procedure CreateNodes;
    procedure SetOptions;
    property TreeList: TcxTreeList read FTreeList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Control: TWinControl; override;
    class function GetStyleSheetClass: TcxCustomStyleSheetClass; override;
    procedure SetStyleSheet(AStyleSheet: TcxCustomStyleSheet); override;
  end;

{ TcxTreeListStyleSheetEditorPreview }

constructor TcxTreeListStyleSheetEditorPreview.Create(AOwner: TComponent);
begin
  FTreeList := TcxTreeList.Create(AOwner);
  CreateData;
end;

destructor TcxTreeListStyleSheetEditorPreview.Destroy;
begin
  FTreeList.Free;
  inherited Destroy;
end;

function TcxTreeListStyleSheetEditorPreview.Control: TWinControl;
begin
  Result := FTreeList;
end;

class function TcxTreeListStyleSheetEditorPreview.GetStyleSheetClass: TcxCustomStyleSheetClass;
begin
  Result := TcxTreeListStyleSheet;
end;

procedure TcxTreeListStyleSheetEditorPreview.SetStyleSheet(
  AStyleSheet: TcxCustomStyleSheet);
begin
  TreeList.Styles.StyleSheet := AStyleSheet;
end;

procedure TcxTreeListStyleSheetEditorPreview.CreateData;
begin
  TreeList.BeginUpdate;
  try
    CreateColumns;
    TreeList.Bands[0].Caption.Text := 'Band 0';
    TreeList.Bands[0].Caption.AlignHorz := taCenter;
    CreateNodes;
    SetOptions;
    finally
    TreeList.EndUpdate;
  end;
end;

procedure TcxTreeListStyleSheetEditorPreview.CreateColumns;
var
  I: Integer;
  AColumn: TcxTreeListColumn;
const
  AColumnDataType: array[0..3] of string =
    ('String', 'Currency', 'Date', 'String');
begin
  for I := 0 to High(AColumnDataType) do
  begin
    AColumn := TreeList.CreateColumn();
    with AColumn do
    begin
      Caption.Text := AColumnDataType[I];
      Caption.AlignHorz := taCenter;
      Caption.AlignVert := vaCenter;
      DataBinding.ValueType := AColumnDataType[I];
      Options.Footer := True;
    end;
  end;
  TreeList.Columns[1].Summary.FooterSummaryItems.Add.Kind := skSum;
  TreeList.Preview.Column := AColumn;
end;

procedure TcxTreeListStyleSheetEditorPreview.CreateNodes;
var
  I: Integer;
  ANode: TcxTreeListNode;
begin
  ANode := nil;
  for I := 0 to 10 do
  begin
    if ((I mod 2) = 0) or ((I mod 3) = 0) then
      ANode := TreeList.AddChild(ANode)
    else
      ANode := TreeList.Add(nil);
    ANode.Values[0] := cxTextDescription;
    ANode.Values[1] := (I + 1) * 100;
    ANode.Values[2] := Date - I;
    ANode.Values[3] := cxPreviewDescription;
  end;
  TreeList.FullExpand;
end;

procedure TcxTreeListStyleSheetEditorPreview.SetOptions;
begin
  with TreeList.OptionsView do
  begin
    Bands := True;
    Indicator := True;
    Headers := True;
    Footer := True;
    ColumnAutoWidth := True;
  end;
  with TreeList.OptionsData do
  begin
    Deleting := False;
    Editing := False;
    Inserting := False;
  end;
  TreeList.OptionsBehavior.IncSearch := True;
  TreeList.Preview.AutoHeight := True;
  TreeList.Preview.Visible := True;
end;

initialization
  RegisterStyleSheetEditorPreview(TcxTreeListStyleSheetEditorPreview);

finalization
  UnregisterStyleSheetEditorPreview(TcxTreeListStyleSheetEditorPreview);

end.
