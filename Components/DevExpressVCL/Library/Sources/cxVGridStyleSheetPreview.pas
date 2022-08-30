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
unit cxVGridStyleSheetPreview;

{$I cxVer.inc}

interface

uses
  Windows, Messages,
  Classes, Controls, SysUtils, cxStyles, cxVGrid;

resourcestring
  cxTextDescription    = 'ABC';
  cxPreviewDescription = 'Check the preview style';

implementation

uses
  cxStyleSheetEditor, cxClasses;

type
  TcxVerticalGridStyleSheetEditorPreview = class(TcxStyleSheetEditorPreview)
  private
    FVerticalGrid: TcxCustomVerticalGrid;
  protected
    procedure CreateData;
    procedure SetOptions;
    property VerticalGrid: TcxCustomVerticalGrid read FVerticalGrid;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Control: TWinControl; override;
    class function GetStyleSheetClass: TcxCustomStyleSheetClass; override;
    procedure SetStyleSheet(AStyleSheet: TcxCustomStyleSheet); override;
  end;

{ TcxVerticalGridStyleSheetEditorPreview }

constructor TcxVerticalGridStyleSheetEditorPreview.Create(AOwner: TComponent);
begin
  FVerticalGrid := TcxVerticalGrid.Create(AOwner);
  CreateData;
end;

destructor TcxVerticalGridStyleSheetEditorPreview.Destroy;
begin
  FVerticalGrid.Free;
  inherited Destroy;
end;

function TcxVerticalGridStyleSheetEditorPreview.Control: TWinControl;
begin
  Result := FVerticalGrid;
end;

class function TcxVerticalGridStyleSheetEditorPreview.GetStyleSheetClass: TcxCustomStyleSheetClass;
begin
  Result := TcxVerticalGridStyleSheet;
end;

procedure TcxVerticalGridStyleSheetEditorPreview.SetStyleSheet(
  AStyleSheet: TcxCustomStyleSheet);
begin
  VerticalGrid.Styles.StyleSheet := AStyleSheet;
end;

procedure TcxVerticalGridStyleSheetEditorPreview.CreateData;
const
  Captions: array[0..3] of string = ('Text edit', 'Currency edit',
    'Spin edit', 'Date edit');
  ValueTypes: array[0..3] of string = ('String', 'Currency', 'Integer', 'Date');

var
  ACategoryRow: TcxCategoryRow;
  AEditorRow: TcxEditorRow;
  I: Integer;
begin
  VerticalGrid.BeginUpdate;
  try
    VerticalGrid.OptionsView.RowHeaderWidth := 150;
    ACategoryRow := TcxCategoryRow(VerticalGrid.Add(TcxCategoryRow));
    with ACategoryRow do
    begin
      Properties.HeaderAlignmentVert := vaCenter;
      Properties.Caption := 'Category Row'
    end;
    for I := 0 to 3 do
    begin
      AEditorRow := TcxEditorRow(VerticalGrid.AddChild(ACategoryRow, TcxEditorRow));
      with AEditorRow do
      begin
        Properties.HeaderAlignmentVert := vaCenter;
        Properties.Caption := Captions[I];
        Properties.DataBinding.ValueType := ValueTypes[I];
        case I of
          0: Properties.Value := 'Some string';
          1: Properties.Value := 123456789;
          2: Properties.Value := 999;
          3: Properties.Value := Date;
        end;
      end;
    end;
    SetOptions;
  finally
    VerticalGrid.EndUpdate;
  end;
end;

procedure TcxVerticalGridStyleSheetEditorPreview.SetOptions;
begin
  with VerticalGrid.OptionsView do
  begin
  end;
  VerticalGrid.OptionsData.Editing := False;
//todo:
//  VerticalGrid.OptionsBehavior.IncSearch := True;
end;

initialization
  RegisterStyleSheetEditorPreview(TcxVerticalGridStyleSheetEditorPreview);

finalization
  UnregisterStyleSheetEditorPreview(TcxVerticalGridStyleSheetEditorPreview);

end.
