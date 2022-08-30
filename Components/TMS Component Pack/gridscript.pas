{***********************************************************************}
{ TGridScript component                                                 }
{ for Delphi 3.0,4.0,5.0 & C++Builder 3.0,4.0,5.0                       }
{ version 1.0, April 2001                                               }
{                                                                       }
{ written by TMS Software                                               }
{            copyright © 2001                                           }
{            Email: info@tmssoftware.com                                }
{            Web: http://www.tmssoftware.com                            }
{                                                                       }
{ The source code is given as is. The author is not responsible         }
{ for any possible damage done due to the use of this code.             }
{ The component can be freely used in any application. The complete     }
{ source code remains property of the author and may not be distributed,}
{ published, given or sold in any form as such. No parts of the source  }
{ code can be included in any other component or application without    }
{ written authorization of the author.                                  }
{***********************************************************************}

unit GridScript;

interface

uses
  atScript, AdvGrid, Classes, Graphics, AdvObj, Windows, Grids;

type

  TPointWrapper = class(TComponent)
  private
    FX: Integer;
    FY: Integer;
  published
    property X: Integer read FX write FX;
    property Y: Integer read FY write FY;
  end;

  TGridScript = class(tAtPascalScripter)
  private
    FGrid: TAdvStringGrid;
    procedure SetGrid(const Value: TAdvStringGrid);
    procedure _Include;
    procedure _Exclude;
  protected
    procedure SetCells;
    procedure GetCells;
    procedure SetFloats;
    procedure GetFloats;
    procedure SetInts;
    procedure GetInts;
    procedure GetSaveFixedCells;
    procedure SetSaveFixedCells;

    procedure LoadFromCSV;
    procedure SaveToCSV;
    procedure SaveToFile;
    procedure SaveToHTML;
    procedure AppendToHTML;
    procedure SaveToASCII;
    procedure AppendToCSV;
    procedure LoadFromFile;
    procedure InsertFromCSV;
    procedure SaveToXLS;
    procedure LoadFromXLS;
    procedure SaveToDOC;
    procedure SaveToXLSSheet;
    procedure LoadFromXLSSheet;
    procedure SaveColSizes;
    procedure LoadColSizes;

    procedure RemoveRows;
    procedure InsertRows;
    procedure RemoveCols;
    procedure InsertCols;

    procedure HideColumn;
    procedure UnhideColumn;
    procedure HideColumns;
    procedure UnhideColumns;
    procedure UnhideColumnsAll;

    procedure HideRow;
    procedure UnhideRow;
    procedure HideRows;
    procedure UnhideRows;
    procedure UnhideRowsAll;

    procedure GroupSum;
    procedure GroupAvg;
    procedure GroupMin;
    procedure GroupMax;
    procedure GroupCount;
    procedure Group;
    procedure UnGroup;

    procedure CutToClipboard;
    procedure CutSelectionToClipboard;
    procedure CopyToClipBoard;
    procedure CopySelectionToClipboard;
    procedure PasteFromClipboard;
    procedure PasteSelectionFromClipboard;

    procedure QSort;
    procedure QSortIndexed;
    procedure QSortGroup;

    procedure AutoSizeCells;
    procedure AutoSizeColumns;
    procedure AutoSizeCol;
    procedure AutoSizeRows;
    procedure AutoSizeRow;

    procedure MergeCols;
    procedure SwapColumns;
    procedure MoveRow;
    procedure MoveColumn;
    procedure SwapRows;

    procedure ClearRect;
    procedure ClearGrid;
    procedure ClearRows;
    procedure ClearCols;
    procedure ClearNormalCells;
    procedure ClearRowSelect;

    procedure SelectRows;
    procedure SelectCols;
    procedure SelectRange;

    procedure SavePrintSettings;
    procedure LoadPrintSettings;
    procedure RandomFill;
    procedure TextFill;
    procedure Zoom;
    procedure AutoNumberCol;
    procedure AutoNumberRow;
    procedure ShowColumnHeaders;
    procedure ClearColumnHeaders;
    procedure ShowRowHeaders;
    procedure ClearRowHeaders;
    procedure HideCellEdit;
    procedure ShowCellEdit;
    procedure StretchRightColumn;
    procedure Print;
    procedure PrintRect;

    procedure AddNode;
    procedure RemoveNode;
    procedure IsNode;
    procedure GetNodeState;
    procedure SetNodeState;
    procedure ExpandNode;
    procedure ContractNode;
    procedure ExpandAll;
    procedure ContractAll;

    procedure ColumnSum;
    procedure ColumnAvg;
    procedure ColumnMin;
    procedure ColumnMax;
    procedure RowSum;
    procedure RowAvg;
    procedure RowMin;
    procedure RowMax;

    procedure ClearComboString;
    procedure AddComboString;
    procedure AddComboStringObject;
    procedure RemoveComboString;
    procedure SetComboSelectionString;
    procedure SetComboSelection;
    procedure GetComboCount;
    procedure GetVersionNr;
    procedure GetVersionString;

    procedure HideSelection;
    procedure UnHideSelection;
    procedure ScrollInView;
    procedure IsSelected;
    procedure ShowInplaceEdit;
    procedure HideInplaceEdit;

    procedure SaveToXML;
    procedure SaveToFixed;
    procedure LoadFromFixed;
    procedure LoadFromStream;
    procedure SaveToStream;

    procedure RemoveSelectedRows;
    procedure HideSelectedRows;
    procedure IsHiddenColumn;
    procedure IsHiddenRow;

    procedure NumHiddenColumns;
    procedure NumHiddenRows;

    procedure RealRowIndex;
    procedure RealColIndex;
    procedure DisplRowIndex;
    procedure DisplColIndex;
    procedure GetRealCol;
    procedure GetRealRow;

    procedure AddCheckBox;
    procedure RemoveCheckBox;
    procedure HasCheckBox;
    procedure HasDataCheckBox;
    procedure GetCheckBoxState;
    procedure SetCheckBoxState;
    procedure ToggleCheckBox;

    procedure AddComment;
    procedure RemoveComment;
    procedure IsComment;

    procedure FindFirst;
    procedure FindNext;

    procedure AddImageIdx;
    procedure RemoveImageIdx;
    procedure GetImageIdx;

    procedure AddRotated;
    procedure RemoveRotated;
    procedure IsRotated;

    procedure RepaintRect;
    procedure RepaintCell;
    procedure RepaintRow;
    procedure RepaintCol;

    procedure GetDelimiter;
    procedure SetDelimiter;

    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Loaded; override;
    procedure Compile; override;
    function Execute( input:variant ):variant; override;
    procedure AddGrid(GridName:string; AGrid: TAdvStringGrid);
  published
    property Grid: TAdvStringGrid read FGrid write SetGrid;
  end;



implementation

{ TGridScript }

procedure TGridScript.Compile;
begin
  inherited;
end;

constructor TGridScript.Create(AOwner: TComponent);
begin
  inherited;
end;

function TGridScript.Execute(input: variant): variant;
begin
  inherited Execute(input);
end;

procedure TGridScript.GetCells;
begin
  ReturnOutputArg(TAdvStringGrid(CurrentObject).Cells[GetInputArgAsInteger(0),GetInputArgAsInteger(1)]);
end;

procedure TGridScript.GetFloats;
begin
  ReturnOutputArg(TAdvStringGrid(CurrentObject).Floats[GetInputArgAsInteger(0),GetInputArgAsInteger(1)]);
end;

procedure TGridScript.GetInts;
begin
  ReturnOutputArg(TAdvStringGrid(CurrentObject).Ints[GetInputArgAsInteger(0),GetInputArgAsInteger(1)]);
end;

procedure TGridScript.GetSaveFixedCells;
begin
  ReturnOutputArg(TAdvStringGrid(CurrentObject).SaveFixedCells);
end;

procedure TGridScript.Loaded;
begin
  inherited;
  {add all public methods & properties used by TAdvStringGrid here}
  with AddDelphiClass(TAdvStringGrid) do
  begin
    AddMethod('Cells',2,tkString,nil,GetCells);
    AddMethod('SetCells',3,tkNone,nil,SetCells);
    AddMethod('Floats',2,tkFloat,nil,GetFloats);
    AddMethod('SetFloats',3,tkNone,nil,SetFloats);
    AddMethod('Ints',2,tkInteger,nil,GetInts);
    AddMethod('SetInts',3,tkNone,nil,SetInts);
    AddMethod('SaveToCSV',1,tkNone,nil,SaveToCSV);
    AddMethod('SaveToFile',1,tkNone,nil,SaveToFile);
    AddMethod('SaveToHTML',1,tkNone,nil,SaveToHTML);
    AddMethod('SaveToASCII',1,tkNone,nil,SaveToAscii);
    AddMethod('SaveToXLS',1,tkNone,nil,SaveToXLS);
    AddMethod('SaveToDOC',1,tkNone,nil,SaveToDoc);
    AddMethod('AppendToHTML',1,tkNone,nil,AppendToHTML);
    AddMethod('AppendToCSV',1,tkNone,nil,AppendToCSV);
    AddMethod('LoadFromFile',1,tkNone,nil,LoadFromFile);
    AddMethod('LoadFromCSV',1,tkNone,nil,LoadFromCSV);
    AddMethod('LoadFromXLS',1,tkNone,nil,LoadFromXLS);
    AddMethod('InsertFromCSV',1,tkNone,nil,InsertFromCSV);
    AddMethod('LoadFromXLSSheet',2,tkNone,nil,LoadFromXLSSheet);
    AddMethod('SaveToXLSSheet',2,tkNone,nil,SaveToXLSSheet);
    AddMethod('SaveColSizes',0,tkNone,nil,SaveColSizes);
    AddMethod('LoadColSizes',0,tkNone,nil,LoadColSizes);

    AddMethod('SaveToXML',4,tkNone,nil,SaveToXML);
    AddMethod('SaveToFixed',2,tkNone,nil,SaveToFixed);
    AddMethod('LoadFromFixed',2,tkNone,nil,LoadFromFixed);
    AddMethod('LoadFromStream',1,tkNone,nil,LoadFromStream);
    AddMethod('SaveToStream',1,tkNone,nil,SaveToStream);

    AddMethod('RemoveRows',2,tkNone,nil,RemoveRows);
    AddMethod('InsertRows',2,tkNone,nil,InsertRows);
    AddMethod('RemoveCols',2,tkNone,nil,RemoveCols);
    AddMethod('InsertCols',2,tkNone,nil,InsertCols);

    AddMethod('HideColumn',1,tkNone,nil,HideColumn);
    AddMethod('UnhideColumn',1,tkNone,nil,UnhideColumn);
    AddMethod('HideColumns',2,tkNone,nil,HideColumns);
    AddMethod('UnhideColumns',2,tkNone,nil,UnhideColumns);
    AddMethod('UnhideColumnsAll',0,tkNone,nil,UnhideColumnsAll);

    AddMethod('HideRow',1,tkNone,nil,HideRow);
    AddMethod('UnhideRow',1,tkNone,nil,UnhideRow);
    AddMethod('HideRows',2,tkNone,nil,HideRows);
    AddMethod('UnhideRows',2,tkNone,nil,UnhideRows);
    AddMethod('UnhideRowsAll',0,tkNone,nil,UnhideRowsAll);

    AddMethod('GroupSum',1,tkNone,nil,GroupSum);
    AddMethod('GroupAvg',1,tkNone,nil,GroupAvg);
    AddMethod('GroupMin',1,tkNone,nil,GroupMin);
    AddMethod('GroupMax',1,tkNone,nil,GroupMax);
    AddMethod('GroupCount',1,tkNone,nil,GroupCount);
    AddMethod('Group',1,tkNone,nil,Group);
    AddMethod('UnGroup',0,tkNone,nil,UnGroup);

    AddMethod('CutToClipboard',0,tkNone,nil,CutToClipboard);
    AddMethod('CutSelectionToClipboard',0,tkNone,nil,CutSelectionToClipboard);
    AddMethod('CopyToClipBoard',0,tkNone,nil,CopyToClipboard);
    AddMethod('CopySelectionToClipboard',0,tkNone,nil,CopySelectionToClipboard);
    AddMethod('PasteFromClipboard',0,tkNone,nil,PasteFromClipboard);
    AddMethod('PasteSelectionFromClipboard',0,tkNone,nil,PasteSelectionFromClipboard);

    AddMethod('QSort',1,tkNone,nil,QSort);
    AddMethod('QSortIndexed',1,tkNone,nil,QSortIndexed);
    AddMethod('QSortGroup',1,tkNone,nil,QSortGroup);

    AddMethod('AutoSizeCells',3,tkNone,nil,AutoSizeCells);
    AddMethod('AutoSizeColumns',2,tkNone,nil,AutoSizeColumns);
    AddMethod('AutoSizeCol',1,tkNone,nil,AutoSizeCol);
    AddMethod('AutoSizeRows',2,tkNone,nil,AutoSizeRows);
    AddMethod('AutoSizeRow',1,tkNone,nil,AutoSizeRow);

    AddMethod('MergeCols',2,tkNone,nil,MergeCols);
    AddMethod('SwapColumns',2,tkNone,nil,SwapColumns);
    AddMethod('MoveRow',2,tkNone,nil,MoveRow);
    AddMethod('MoveColumn',2,tkNone,nil,MoveColumn);
    AddMethod('SwapRows',2,tkNone,nil,SwapRows);

    AddMethod('ClearRect',4,tkNone,nil,ClearRect);
    AddMethod('Clear',0,tkNone,nil,ClearGrid);
    AddMethod('ClearRows',2,tkNone,nil,ClearRows);
    AddMethod('ClearCols',2,tkNone,nil,ClearCols);
    AddMethod('ClearNormalCells',0,tkNone,nil,ClearNormalCells);
    AddMethod('ClearRowSelect',0,tkNone,nil,ClearRowSelect);

    AddMethod('SelectRows',4,tkNone,nil,SelectRows);
    AddMethod('SelectCols',2,tkNone,nil,SelectCols);
    AddMethod('SelectRange',2,tkNone,nil,SelectRange);

    AddMethod('ShowColumnHeaders',0,tkNone,nil,ShowColumnHeaders);
    AddMethod('ClearColumnHeaders',0,tkNone,nil,ClearColumnHeaders);
    AddMethod('ShowRowHeaders',0,tkNone,nil,ShowRowHeaders);
    AddMethod('ClearRowHeaders',0,tkNone,nil,ClearRowHeaders);
    AddMethod('HideCellEdit',0,tkNone,nil,HideCellEdit);
    AddMethod('ShowCellEdit',0,tkNone,nil,ShowCellEdit);
    AddMethod('StretchRightColumn',0,tkNone,nil,StretchRightColumn);

    AddMethod('SavePrintSettings',2,tkNone,nil,SavePrintSettings);
    AddMethod('LoadPrintSettings',2,tkNone,nil,LoadPrintSettings);
    AddMethod('RandomFill',2,tkNone,nil,RandomFill);
    AddMethod('TextFill',2,tkNone,nil,TextFill);
    AddMethod('Zoom',1,tkNone,nil,Zoom);
    AddMethod('AutoNumberCol',1,tkNone,nil,AutoNumberCol);
    AddMethod('AutoNumberRow',1,tkNone,nil,AutoNumberRow);
    AddMethod('Print',0,tkNone,nil,Print);
    AddMethod('PrintRect',4,tkNone,nil,PrintRect);

    AddMethod('AddNode',2,tkNone,nil,AddNode);;
    AddMethod('RemoveNode',1,tkNone,nil,RemoveNode);
    AddMethod('IsNode',1,tkInteger,nil,IsNode);
    AddMethod('GetNodeState',1,tkInteger,nil,GetNodeState);
    AddMethod('SetNodeState',2,tkNone,nil,SetNodeState);
    AddMethod('ExpandNode',1,tkNone,nil,ExpandNode);
    AddMethod('ContractNode',1,tkNone,nil,ContractNode);
    AddMethod('ExpandAll',0,tkNone,nil,ExpandAll);
    AddMethod('ContractAll',0,tkNone,nil,ContractAll);

    AddMethod('ColumnSum',3,tkFloat,nil,ColumnSum);
    AddMethod('ColumnAvg',3,tkFloat,nil,ColumnAvg);
    AddMethod('ColumnMin',3,tkFloat,nil,ColumnMin);
    AddMethod('ColumnMax',3,tkFloat,nil,ColumnMax);
    AddMethod('RowSum',3,tkFloat,nil,RowSum);
    AddMethod('RowAvg',3,tkFloat,nil,RowAvg);
    AddMethod('RowMin',3,tkFloat,nil,RowMin);
    AddMethod('RowMax',3,tkFloat,nil,RowMax);

    AddMethod('ClearComboString',0,tkNone,nil,ClearComboString);
    AddMethod('AddComboString',1,tkNone,nil,AddComboString);
    AddMethod('AddComboStringObject',2,tkNone,nil,AddComboStringObject);
    AddMethod('RemoveComboString',1,tkInteger,nil,RemoveComboString);
    AddMethod('SetComboSelectionString',1,tkInteger,nil,SetComboSelectionString);
    AddMethod('SetComboSelection',1,tkNone,nil,SetComboSelection);
    AddMethod('GetComboCount',0,tkInteger,nil,GetComboCount);
    AddMethod('GetVersionNr',0,tkInteger,nil,GetVersionNr);
    AddMethod('GetVersionString',0,tkstring,nil,GetVersionString);

    AddMethod('HideSelection',0,tkNone,nil,HideSelection);
    AddMethod('UnHideSelection',0,tkNone,nil,HideSelection);
    AddMethod('ScrollInView',2,tkNone,nil,ScrollInView);
    AddMethod('IsSelected',2,tkInteger,nil,IsSelected);
    AddMethod('ShowInplaceEdit',0,tkNone,nil,ShowInplaceEdit);
    AddMethod('HideInplaceEdit',0,tkNone,nil,ShowInplaceEdit);

    AddMethod('RemoveSelectedRows',0,tkNone,nil,RemoveSelectedRows);
    AddMethod('HideSelectedRows',0,tkNone,nil,HideSelectedRows);
    AddMethod('IsHiddenColumn',1,tkInteger,nil,IsHiddenColumn);
    AddMethod('IsHiddenRow',1,tkInteger,nil,IsHiddenRow);
    AddMethod('NumHiddenColumns',0,tkInteger,nil,NumHiddenColumns);
    AddMethod('NumHiddenRows',0,tkInteger,nil,NumHiddenRows);

    AddMethod('RealRowIndex',1,tkInteger,nil,RealRowIndex);
    AddMethod('RealColIndex',1,tkInteger,nil,RealColIndex);
    AddMethod('DisplRowIndex',1,tkInteger,nil,DisplRowIndex);
    AddMethod('DisplColIndex',1,tkInteger,nil,DisplColIndex);
    AddMethod('GetRealCol',0,tkInteger,nil,GetRealCol);
    AddMethod('GetRealRow',0,tkInteger,nil,GetRealRow);

    AddMethod('AddCheckBox',4,tkNone,nil,AddCheckBox);
    AddMethod('RemoveCheckBox',2,tkNone,nil,RemoveCheckBox);
    AddMethod('HasCheckBox',4,tkInteger,nil,HasCheckBox);
    AddMethod('HasDataCheckBox',2,tkInteger,nil,HasDataCheckBox);
    AddMethod('GetCheckBoxState',2,tkInteger,nil,GetCheckBoxState);
    AddMethod('SetCheckBoxState',3,tkNone,nil,SetCheckBoxState);
    AddMethod('ToggleCheckBox',2,tkNone,nil,ToggleCheckBox);

    AddMethod('AddComment',3,tkNone,nil,AddComment);
    AddMethod('RemoveComment',2,tkNone,nil,RemoveComment);
    AddMethod('IsComment',2,tkInteger,nil,IsComment);

    AddMethod('FindFirst',2,tkVariant,nil,FindFirst);
    AddMethod('FindNext',0,tkVariant,nil,FindNext);

    AddMethod('AddImageIdx',5,tkNone,nil,AddImageIdx);
    AddMethod('RemoveImageIdx',2,tkNone,nil,RemoveImageIdx);
    AddMethod('GetImageIdx',2,tkInteger,nil,GetImageIdx);

    AddMethod('AddRotated',4,tkNone,nil,AddRotated);
    AddMethod('RemoveRotated',2,tkNone,nil,RemoveRotated);
    AddMethod('IsRotated',2,tkInteger,nil,IsRotated);

    AddMethod('RepaintRect',4,tkNone,nil,RepaintRect);
    AddMethod('RepaintCell',2,tkNone,nil,RepaintCell);
    AddMethod('RepaintRow',1,tkNone,nil,RepaintRow);
    AddMethod('RepaintCol',1,tkNone,nil,RepaintCol);

    AddMethod('Include',2,tkInteger,nil,_Include);
    AddMethod('Exclude',2,tkInteger,nil,_Exclude);

    AddMethod('GetDelimiter',0,tkChar,nil,GetDelimiter);
    AddMethod('SetDelimiter',1,tkNone,nil,SetDelimiter);

    AddBooleanProp('SaveFixedCells',GetSaveFixedCells,SetSaveFixedCells);
  end;

  {add all constants used by TAdvStringGrid here}
  SystemLibrary.AddConstant('clScrollBar',clScrollBar);
  SystemLibrary.AddConstant('clBackground',clBackground);
  SystemLibrary.AddConstant('clActiveCaption',clActiveCaption);
  SystemLibrary.AddConstant('clInactiveCaption',clInactiveCaption);
  SystemLibrary.AddConstant('clMenu',clMenu);
  SystemLibrary.AddConstant('clWindow',clWindow);
  SystemLibrary.AddConstant('clWindowFrame',clWindowFrame);
  SystemLibrary.AddConstant('clMenuText',clMenuText);
  SystemLibrary.AddConstant('clWindowText',clWindowText);
  SystemLibrary.AddConstant('clCaptionText',clCaptionText);
  SystemLibrary.AddConstant('clActiveBorder',clActiveBorder);
  SystemLibrary.AddConstant('clInactiveBorder',clInactiveBorder);
  SystemLibrary.AddConstant('clAppWorkSpace',clAppWorkSpace);
  SystemLibrary.AddConstant('clHighlight',clHighlight);
  SystemLibrary.AddConstant('clHighlightText',clHighlightText);
  SystemLibrary.AddConstant('clBtnFace',clBtnFace);
  SystemLibrary.AddConstant('clBtnShadow',clBtnShadow);
  SystemLibrary.AddConstant('clGrayText',clGrayText);
  SystemLibrary.AddConstant('clBtnText',clBtnText);
  SystemLibrary.AddConstant('clInactiveCaptionText',clInactiveCaptionText);
  SystemLibrary.AddConstant('clBtnHighlight',clBtnHighlight);
  SystemLibrary.AddConstant('cl3DDkShadow',cl3DDkShadow);
  SystemLibrary.AddConstant('cl3DLight',cl3DLight);
  SystemLibrary.AddConstant('clInfoText',clInfoText);
  SystemLibrary.AddConstant('clInfoBk',clInfoBk);

  SystemLibrary.AddConstant('clBlack',clBlack);
  SystemLibrary.AddConstant('clMaroon',clMaroon);
  SystemLibrary.AddConstant('clGreen',clGreen);
  SystemLibrary.AddConstant('clOlive',clOlive);
  SystemLibrary.AddConstant('clNavy',clNavy);
  SystemLibrary.AddConstant('clPurple',clPurple);
  SystemLibrary.AddConstant('clTeal',clTeal);
  SystemLibrary.AddConstant('clGray',clGray);
  SystemLibrary.AddConstant('clSilver',clSilver);
  SystemLibrary.AddConstant('clRed',clRed);
  SystemLibrary.AddConstant('clLime',clLime);
  SystemLibrary.AddConstant('clYellow',clYellow);
  SystemLibrary.AddConstant('clBlue',clBlue);
  SystemLibrary.AddConstant('clFuchsia',clFuchsia);
  SystemLibrary.AddConstant('clAqua',clAqua);
  SystemLibrary.AddConstant('clLtGray',clLtGray);
  SystemLibrary.AddConstant('clDkGray',clDkGray);
  SystemLibrary.AddConstant('clWhite',clWhite);
  SystemLibrary.AddConstant('clNone',clNone);
  SystemLibrary.AddConstant('clDefault',clDefault);

  SystemLibrary.AddConstant('edNormal',edNormal);
  SystemLibrary.AddConstant('edSpinEdit',edSpinEdit);
  SystemLibrary.AddConstant('edComboEdit',edComboEdit);
  SystemLibrary.AddConstant('edComboList',edComboList);
  SystemLibrary.AddConstant('edEditBtn',edEditBtn);
  SystemLibrary.AddConstant('edCheckBox',edCheckBox);
  SystemLibrary.AddConstant('edDateEdit',edDateEdit);
  SystemLibrary.AddConstant('edDateEditUpDown',edDateEditUpDown);
  SystemLibrary.AddConstant('edTimeEdit',edTimeEdit);
  SystemLibrary.AddConstant('edButton',edButton);
  SystemLibrary.AddConstant('edDataCheckBox',edDataCheckBox);
  SystemLibrary.AddConstant('edNumeric',edNumeric);
  SystemLibrary.AddConstant('edPositiveNumeric',edPositiveNumeric);
  SystemLibrary.AddConstant('edFloat',edFloat);
  SystemLibrary.AddConstant('edCapital',edCapital);
  SystemLibrary.AddConstant('edMixedCase',edMixedCase);
  SystemLibrary.AddConstant('edPassWord',edPassword);
  SystemLibrary.AddConstant('edUnitEditBtn',edUnitEditBtn);
  SystemLibrary.AddConstant('edLowerCase',edLowerCase);
  SystemLibrary.AddConstant('edUpperCase',edUpperCase);
  SystemLibrary.AddConstant('edFloatSpinEdit',edFloatSpinEdit);
  SystemLibrary.AddConstant('edTimeSpinEdit',edTimeSpinEdit);
  SystemLibrary.AddConstant('edDateSpinEdit',edDateSpinEdit);
  SystemLibrary.AddConstant('edNumericEditBtn',edNumericEditBtn);
  SystemLibrary.AddConstant('edFloatEditBtn',edFloatEditBtn);
  SystemLibrary.AddConstant('edCustom',edCustom);

  SystemLibrary.AddConstant('shNone',shNone);
  SystemLibrary.AddConstant('shVertical',shVertical);
  SystemLibrary.AddConstant('shHorizontal',shHorizontal);
  SystemLibrary.AddConstant('shBoth',shBoth);

  SystemLibrary.AddConstant('ssAutomatic',ssAutomatic);
  SystemLibrary.AddConstant('ssAlphabetic',ssAlphabetic);
  SystemLibrary.AddConstant('ssNumeric',ssNumeric);
  SystemLibrary.AddConstant('ssDate',ssDate);
  SystemLibrary.AddConstant('ssAlphaNoCase',ssAlphaNoCase);
  SystemLibrary.AddConstant('ssAlphaCase',ssAlphaCase);
  SystemLibrary.AddConstant('ssShortDateEU',ssShortDateEU);
  SystemLibrary.AddConstant('ssShortDateUS',ssShortDateUS);
  SystemLibrary.AddConstant('ssCustom',ssCustom);
  SystemLibrary.AddConstant('ssFinancial',ssFinancial);
  SystemLibrary.AddConstant('ssAnsiAlphaCase',ssAnsiAlphaCase);
  SystemLibrary.AddConstant('ssAnsiAlphaNoCase',ssAnsiAlphaNoCase);
  SystemLibrary.AddConstant('ssRaw',ssRaw);
  SystemLibrary.AddConstant('ssHTML',ssHTML);
  SystemLibrary.AddConstant('ssImages',ssImages);

  SystemLibrary.AddConstant('ppNone',ppNone);
  SystemLibrary.AddConstant('ppTopLeft',ppTopLeft);
  SystemLibrary.AddConstant('ppTopRight',ppTopRight);
  SystemLibrary.AddConstant('ppTopCenter',ppTopCenter);
  SystemLibrary.AddConstant('ppBottomLeft',ppBottomLeft);
  SystemLibrary.AddConstant('ppBottomRight',ppBottomRight);
  SystemLibrary.AddConstant('ppBottomCenter',ppBottomCenter);

  SystemLibrary.AddConstant('pbNoBorder',pbNoBorder);
  SystemLibrary.AddConstant('pbSingle',pbSingle);
  SystemLibrary.AddConstant('pbDouble',pbDouble);
  SystemLibrary.AddConstant('pbVertical',pbVertical);
  SystemLibrary.AddConstant('pbHorizontal',pbHorizontal);
  SystemLibrary.AddConstant('pbAround',pbAround);
  SystemLibrary.AddConstant('pbAroundVertical',pbAroundVertical);
  SystemLibrary.AddConstant('pbAroundHorizontal',pbAroundHorizontal);
  SystemLibrary.AddConstant('pbCustom',pbCustom);

  SystemLibrary.AddConstant('cbTop',cbTop);
  SystemLibrary.AddConstant('cbLeft',cbLeft);
  SystemLibrary.AddConstant('cbRight',cbRight);
  SystemLibrary.AddConstant('cbBottom',cbBottom);

  SystemLibrary.AddConstant('sdAscending',sdAscending);
  SystemLibrary.AddConstant('sdDesending',sdDescending);

  SystemLibrary.AddConstant('adLeftRight',adLeftRight);
  SystemLibrary.AddConstant('adTopBottom',adTopBottom);

  SystemLibrary.AddConstant('ipVertical',ipVertical);
  SystemLibrary.AddConstant('ipHorizontal',ipHorizontal);
  SystemLibrary.AddConstant('ipBoth',ipBoth);
  SystemLibrary.AddConstant('ipNone',ipNone);

  SystemLibrary.AddConstant('pInsertBefore',pInsertBefore);
  SystemLibrary.AddConstant('pInsertAfter',pInsertAfter);

  SystemLibrary.AddConstant('ssNormal',ssNormal);
  SystemLibrary.AddConstant('ssFlat',ssFlat);
  SystemLibrary.AddConstant('ssEncarta',ssEncarta);

  SystemLibrary.AddConstant('vtaCenter',vtaCenter);
  SystemLibrary.AddConstant('vtaTop',vtaTop);
  SystemLibrary.AddConstant('vtaBottom',vtaBottom);

  SystemLibrary.AddConstant('fnMatchCase',fnMatchCase);
  SystemLibrary.AddConstant('fnMatchFull',fnMatchFull);
  SystemLibrary.AddConstant('fnMatchRegular',fnMatchRegular);
  SystemLibrary.AddConstant('fnDirectionLeftRight',fnDirectionLeftRight);
  SystemLibrary.AddConstant('fnMatchStart',fnMatchStart);
  SystemLibrary.AddConstant('fnFindInCurrentRow',fnFindInCurrentRow);
  SystemLibrary.AddConstant('fnFindInCurrentCol',fnFindInCurrentCol);
  SystemLibrary.AddConstant('fnIncludeFixed',fnIncludeFixed);
  SystemLibrary.AddConstant('fnAutoGoto',fnAutoGoto);
  SystemLibrary.AddConstant('fnIgnoreHTMLTags',fnIgnoreHTMLTags);
  SystemLibrary.AddConstant('fnBackward',fnBackward);

  SystemLibrary.AddConstant('haLeft',haLeft);
  SystemLibrary.AddConstant('haRight',haRight);
  SystemLibrary.AddConstant('haCenter',haCenter);
  SystemLibrary.AddConstant('haBeforeText',haBeforeText);
  SystemLibrary.AddConstant('haAferText',haAfterText);
  SystemLibrary.AddConstant('haFull',haFull);

  SystemLibrary.AddConstant('vaTop',vaTop);
  SystemLibrary.AddConstant('vaBottom',vaBottom);
  SystemLibrary.AddConstant('vaCenter',vaCenter);
  SystemLibrary.AddConstant('vaUnderText',vaUnderText);
  SystemLibrary.AddConstant('vaAboveText',vaAboveText);
  SystemLibrary.AddConstant('vaFull',vaFull);

  SystemLibrary.AddConstant('fpNever',fpNever);
  SystemLibrary.AddConstant('fpGrow',fpGrow);
  SystemLibrary.AddConstant('fpShrink',fpShrink);
  SystemLibrary.AddConstant('fpAlways',fpAlways);
  SystemLibrary.AddConstant('fpCustom',fpCustom);

  SystemLibrary.AddConstant('noStretch',noStretch);
  SystemLibrary.AddConstant('Stretch',Stretch);
  SystemLibrary.AddConstant('StretchWithAspectRatio',StretchWithAspectRatio);
  SystemLibrary.AddConstant('Shrink',Shrink);
  SystemLibrary.AddConstant('ShrinkWithAspectRatio',ShrinkWithAspectRatio);

  SystemLibrary.AddConstant('bdTile',bdTile);
  SystemLibrary.AddConstant('bdFixed',bdFixed);

  SystemLibrary.AddConstant('bcNormal',bcNormal);
  SystemLibrary.AddConstant('bcFixed',bcFixed);

  SystemLibrary.AddConstant('esInplace',esInplace);
  SystemLibrary.AddConstant('esPopup',esPopup);

  SystemLibrary.AddConstant('False',False);
  SystemLibrary.AddConstant('True',True);

end;

procedure TGridScript.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (aOperation=opRemove) and (aComponent=FGrid) then FGrid := Nil;
  inherited;
end;

procedure TGridScript.LoadFromCSV;
begin
  TAdvStringGrid(CurrentObject).LoadFromCSV(GetInputArgAsString(0));
end;

procedure TGridScript.LoadFromFile;
begin
  TAdvStringGrid(CurrentObject).LoadFromFile(GetInputArgAsString(0));
end;

procedure TGridScript.LoadFromXLS;
begin
  TAdvStringGrid(CurrentObject).LoadFromXLS(GetInputArgAsString(0));
end;

procedure TGridScript.LoadFromXLSSheet;
begin
  TAdvStringGrid(CurrentObject).LoadFromXLSSheet(GetInputArgAsString(0),GetInputArgAsString(1));
end;

procedure TGridScript.SaveToCSV;
begin
  TAdvStringGrid(CurrentObject).SaveToCSV(GetInputArgAsString(0));
end;

procedure TGridScript.SaveToFile;
begin
  TAdvStringGrid(CurrentObject).SaveToFile(GetInputArgAsString(0));
end;

procedure TGridScript.SaveToDoc;
begin
  TAdvStringGrid(CurrentObject).SaveToDoc(GetInputArgAsString(0));
end;

procedure TGridScript.SaveToHTML;
begin
  TAdvStringGrid(CurrentObject).SaveToHTML(GetInputArgAsString(0));
end;

procedure TGridScript.SaveToXLS;
begin
  TAdvStringGrid(CurrentObject).SaveToXLS(GetInputArgAsString(0));
end;

procedure TGridScript.SaveToXLSSheet;
begin
  TAdvStringGrid(CurrentObject).SaveToXLSSheet(GetInputArgAsString(0),GetInputArgAsString(1));
end;

procedure TGridScript.SaveToAscii;
begin
  TAdvStringGrid(CurrentObject).SaveToAscii(GetInputArgAsString(0));
end;

procedure TGridScript.AppendToCSV;
begin
  TAdvStringGrid(CurrentObject).AppendToCSV(GetInputArgAsString(0));
end;

procedure TGridScript.AppendToHTML;
begin
  TAdvStringGrid(CurrentObject).AppendToHTML(GetInputArgAsString(0));
end;

procedure TGridScript.InsertFromCSV;
begin
  TAdvStringGrid(CurrentObject).InsertFromCSV(GetInputArgAsString(0));
end;

procedure TGridScript.SaveColSizes;
begin
  TAdvStringGrid(CurrentObject).SaveColSizes;
end;

procedure TGridScript.LoadColSizes;
begin
  TAdvStringGrid(CurrentObject).LoadColSizes;
end;

procedure TGridScript.InsertCols;
begin
  TAdvStringGrid(CurrentObject).InsertCols(GetInputArgAsInteger(0),GetInputArgAsInteger(1));
end;

procedure TGridScript.InsertRows;
begin
  TAdvStringGrid(CurrentObject).InsertRows(GetInputArgAsInteger(0),GetInputArgAsInteger(1));
end;

procedure TGridScript.RemoveCols;
begin
  TAdvStringGrid(CurrentObject).RemoveCols(GetInputArgAsInteger(0),GetInputArgAsInteger(1));
end;

procedure TGridScript.RemoveRows;
begin
  TAdvStringGrid(CurrentObject).RemoveRows(GetInputArgAsInteger(0),GetInputArgAsInteger(1));
end;

procedure TGridScript.HideColumn;
begin
  TAdvStringGrid(CurrentObject).HideColumn(GetInputArgAsInteger(0));
end;

procedure TGridScript.UnhideColumn;
begin
  TAdvStringGrid(CurrentObject).UnhideColumn(GetInputArgAsInteger(0));
end;

procedure TGridScript.HideColumns;
begin
  TAdvStringGrid(CurrentObject).HideColumns(GetInputArgAsInteger(0),GetInputArgAsInteger(1));
end;

procedure TGridScript.UnhideColumns;
begin
  TAdvStringGrid(CurrentObject).UnhideColumns(GetInputArgAsInteger(0),GetInputArgAsInteger(1));
end;

procedure TGridScript.UnhideColumnsAll;
begin
  TAdvStringGrid(CurrentObject).UnhideColumnsAll;
end;

procedure TGridScript.HideRow;
begin
  TAdvStringGrid(CurrentObject).HideRow(GetInputArgAsInteger(0));
end;

procedure TGridScript.UnhideRow;
begin
  TAdvStringGrid(CurrentObject).UnhideRow(GetInputArgAsInteger(0));
end;

procedure TGridScript.HideRows;
begin
  TAdvStringGrid(CurrentObject).HideRows(GetInputArgAsInteger(0),GetInputArgAsInteger(1));
end;

procedure TGridScript.UnhideRows;
begin
  TAdvStringGrid(CurrentObject).UnhideRows(GetInputArgAsInteger(0),GetInputArgAsInteger(1));
end;

procedure TGridScript.UnhideRowsAll;
begin
  TAdvStringGrid(CurrentObject).UnhideRowsAll;
end;

procedure TGridScript.GroupSum;
begin
  TAdvStringGrid(CurrentObject).GroupSum(GetInputArgAsInteger(0));
end;

procedure TGridScript.GroupAvg;
begin
  TAdvStringGrid(CurrentObject).GroupAvg(GetInputArgAsInteger(0));
end;

procedure TGridScript.GroupMin;
begin
  TAdvStringGrid(CurrentObject).GroupMin(GetInputArgAsInteger(0));
end;

procedure TGridScript.GroupMax;
begin
  TAdvStringGrid(CurrentObject).GroupMax(GetInputArgAsInteger(0));
end;

procedure TGridScript.GroupCount;
begin
  TAdvStringGrid(CurrentObject).GroupCount(GetInputArgAsInteger(0));
end;

procedure TGridScript.UnGroup;
begin
  TAdvStringGrid(CurrentObject).UnGroup;
end;

procedure TGridScript.Group;
begin
  TAdvStringGrid(CurrentObject).Group(GetInputArgAsInteger(0));
end;


procedure TGridScript.SetCells;
begin
  TAdvStringGrid(CurrentObject).Cells[GetInputArgAsInteger(0),GetInputArgAsInteger(1)]:=GetInputArgAsString(2);
end;

procedure TGridScript.SetFloats;
begin
  TAdvStringGrid(CurrentObject).Floats[GetInputArgAsInteger(0),GetInputArgAsInteger(1)]:=GetInputArgAsFloat(2);
end;

procedure TGridScript.SetGrid(const Value: TAdvStringGrid);
begin
  FGrid := Value;
  if Assigned(FGrid) then
    SystemLibrary.AddObject('Grid', FGrid);
end;

procedure TGridScript.AddGrid(GridName: string; AGrid: TAdvStringGrid);
begin
  if Assigned(FGrid) then
    SystemLibrary.AddObject(GridName, AGrid);
end;

procedure TGridScript.SetInts;
begin
  TAdvStringGrid(CurrentObject).Ints[GetInputArgAsInteger(0),GetInputArgAsInteger(1)]:=GetInputArgAsInteger(2);
end;

procedure TGridScript.SetSaveFixedCells;
begin
  TAdvStringGrid(CurrentObject).SaveFixedCells := GetInputArgAsBoolean(0);
end;

procedure TGridScript.CopySelectionToClipboard;
begin
  TAdvStringGrid(CurrentObject).CopySelectionToClipboard;
end;

procedure TGridScript.CopyToClipBoard;
begin
  TAdvStringGrid(CurrentObject).CopyToClipboard;
end;

procedure TGridScript.CutSelectionToClipboard;
begin
  TAdvStringGrid(CurrentObject).CutSelectionToClipboard;
end;

procedure TGridScript.CutToClipboard;
begin
  TAdvStringGrid(CurrentObject).CutToClipboard;
end;

procedure TGridScript.PasteFromClipboard;
begin
  TAdvStringGrid(CurrentObject).PasteFromClipboard;
end;

procedure TGridScript.PasteSelectionFromClipboard;
begin
  TAdvStringGrid(CurrentObject).PasteSelectionFromClipboard;
end;

procedure TGridScript.QSort;
begin
  TAdvStringGrid(CurrentObject).QSort;
end;

procedure TGridScript.QSortGroup;
begin
  TAdvStringGrid(CurrentObject).QSortGroup;
end;

procedure TGridScript.QSortIndexed;
begin
  TAdvStringGrid(CurrentObject).QSortIndexed;
end;

procedure TGridScript.AutoSizeCells;
begin
  TAdvStringGrid(CurrentObject).AutoSizeCells(GetInputArgAsBoolean(0),GetInputArgAsInteger(1),GetInputArgAsInteger(2));
end;

procedure TGridScript.AutoSizeCol;
begin
  TAdvStringGrid(CurrentObject).AutoSizeCol(GetInputArgAsInteger(0));
end;

procedure TGridScript.AutoSizeColumns;
begin
  TAdvStringGrid(CurrentObject).AutoSizeColumns(GetInputArgAsBoolean(0),GetInputArgAsInteger(1));
end;

procedure TGridScript.AutoSizeRow;
begin
  TAdvStringGrid(CurrentObject).AutoSizeRow(GetInputArgAsInteger(0));
end;

procedure TGridScript.AutoSizeRows;
begin
  TAdvStringGrid(CurrentObject).AutoSizeRows(GetInputArgAsBoolean(0),GetInputArgAsInteger(1));
end;

procedure TGridScript.MergeCols;
begin
  TAdvStringGrid(CurrentObject).MergeCols(GetInputArgAsInteger(0),GetInputArgAsInteger(1));
end;

procedure TGridScript.MoveColumn;
begin
  TAdvStringGrid(CurrentObject).MoveColumn(GetInputArgAsInteger(0),GetInputArgAsInteger(1));
end;

procedure TGridScript.MoveRow;
begin
  TAdvStringGrid(CurrentObject).MoveRow(GetInputArgAsInteger(0),GetInputArgAsInteger(1));
end;

procedure TGridScript.SwapColumns;
begin
  TAdvStringGrid(CurrentObject).SwapColumns(GetInputArgAsInteger(0),GetInputArgAsInteger(1));
end;

procedure TGridScript.SwapRows;
begin
  TAdvStringGrid(CurrentObject).SwapRows(GetInputArgAsInteger(0),GetInputArgAsInteger(1));
end;

procedure TGridScript.ClearGrid;
begin
  TAdvStringGrid(CurrentObject).Clear;
end;

procedure TGridScript.ClearCols;
begin
  TAdvStringGrid(CurrentObject).ClearCols(GetInputArgAsInteger(0),GetInputArgAsInteger(1));
end;

procedure TGridScript.ClearNormalCells;
begin
  TAdvStringGrid(CurrentObject).ClearNormalCells;
end;

procedure TGridScript.ClearRect;
begin
  TAdvStringGrid(CurrentObject).ClearRect(GetInputArgAsInteger(0),GetInputArgAsInteger(1),
    GetInputArgAsInteger(2),GetInputArgAsInteger(3));
end;

procedure TGridScript.ClearRows;
begin
  TAdvStringGrid(CurrentObject).ClearRows(GetInputArgAsInteger(0),GetInputArgAsInteger(1));
end;

procedure TGridScript.ClearRowSelect;
begin
  TAdvStringGrid(CurrentObject).ClearRowSelect;
end;

procedure TGridScript.SelectCols;
begin
  TAdvStringGrid(CurrentObject).SelectCols(GetInputArgAsInteger(0),GetInputArgAsInteger(1));
end;

procedure TGridScript.SelectRange;
begin
  TAdvStringGrid(CurrentObject).SelectRange(GetInputArgAsInteger(0),GetInputArgAsInteger(1),
    GetInputArgAsInteger(2),GetInputArgAsInteger(3));
end;

procedure TGridScript.SelectRows;
begin
  TAdvStringGrid(CurrentObject).SelectRows(GetInputArgAsInteger(0),GetInputArgAsInteger(1));
end;

procedure TGridScript.AutoNumberCol;
begin
  TAdvStringGrid(CurrentObject).AutoNumberCol(GetInputArgAsInteger(0));
end;

procedure TGridScript.AutoNumberRow;
begin
  TAdvStringGrid(CurrentObject).AutoNumberRow(GetInputArgAsInteger(0));
end;

procedure TGridScript.ClearColumnHeaders;
begin
  TAdvStringGrid(CurrentObject).ClearColumnHeaders;
end;

procedure TGridScript.ClearRowHeaders;
begin
  TAdvStringGrid(CurrentObject).ClearRowHeaders;
end;

procedure TGridScript.HideCellEdit;
begin
  TAdvStringGrid(CurrentObject).HideCellEdit;
end;

procedure TGridScript.LoadPrintSettings;
begin
  TAdvStringGrid(CurrentObject).LoadPrintSettings(GetInputArgAsString(0),GetInputArgAsString(1));
end;

procedure TGridScript.RandomFill;
begin
  TAdvStringGrid(CurrentObject).RandomFill(GetInputArgAsBoolean(0),GetInputArgAsInteger(1));
end;

procedure TGridScript.TextFill;
begin
  TAdvStringGrid(CurrentObject).TextFill(GetInputArgAsBoolean(0),GetInputArgAsString(1));
end;

procedure TGridScript.SavePrintSettings;
begin
  TAdvStringGrid(CurrentObject).SavePrintSettings(GetInputArgAsString(0),GetInputArgAsString(1));
end;

procedure TGridScript.ShowCellEdit;
begin
  TAdvStringGrid(CurrentObject).ShowCellEdit;
end;

procedure TGridScript.ShowColumnHeaders;
begin
  TAdvStringGrid(CurrentObject).ShowColumnHeaders;
end;

procedure TGridScript.ShowRowHeaders;
begin
  TAdvStringGrid(CurrentObject).ShowRowHeaders;
end;

procedure TGridScript.StretchRightColumn;
begin
  TAdvStringGrid(CurrentObject).StretchRightColumn;
end;

procedure TGridScript.Zoom;
begin
  TAdvStringGrid(CurrentObject).Zoom(GetInputArgAsInteger(0));
end;

procedure TGridScript.Print;
begin
  TAdvStringGrid(CurrentObject).Print;
end;

procedure TGridScript.PrintRect;
var
  gr: TGridRect;
begin
  gr.Left := GetInputArgAsInteger(0);
  gr.Top := GetInputArgAsInteger(1);
  gr.Right := GetInputArgAsInteger(2);
  gr.Bottom := GetInputArgAsInteger(3);

  TAdvStringGrid(CurrentObject).PrintRect(gr);
end;

procedure TGridScript.AddNode;
begin
  TAdvStringGrid(CurrentObject).AddNode(GetInputArgAsInteger(0),GetInputArgAsInteger(1));
end;

procedure TGridScript.ContractAll;
begin
  TAdvStringGrid(CurrentObject).ContractAll;
end;

procedure TGridScript.ContractNode;
begin
  TAdvStringGrid(CurrentObject).ContractNode(GetInputArgAsInteger(0));
end;

procedure TGridScript.ExpandAll;
begin
  TAdvStringGrid(CurrentObject).ExpandAll;
end;

procedure TGridScript.ExpandNode;
begin
  TAdvStringGrid(CurrentObject).ExpandNode(GetInputArgAsInteger(0));
end;

procedure TGridScript.GetNodeState;
begin
  ReturnOutputArg(TAdvStringGrid(CurrentObject).GetNodeState(GetInputArgAsInteger(0)));
end;

procedure TGridScript.IsNode;
begin
  ReturnOutputArg(TAdvStringGrid(CurrentObject).IsNode(GetInputArgAsInteger(0)));
end;

procedure TGridScript.RemoveNode;
begin
  TAdvStringGrid(CurrentObject).RemoveNode(GetInputArgAsInteger(0));
end;

procedure TGridScript.SetNodeState;
begin
  TAdvStringGrid(CurrentObject).SetNodeState(GetInputArgAsInteger(0),GetInputArgAsBoolean(0));
end;

procedure TGridScript.ColumnAvg;
begin
  ReturnOutputArg(TAdvStringGrid(CurrentObject).ColumnAvg(GetInputArgAsInteger(0),
    GetInputArgAsInteger(1),GetInputArgAsInteger(2)));
end;

procedure TGridScript.ColumnMax;
begin
  ReturnOutputArg(TAdvStringGrid(CurrentObject).ColumnMax(GetInputArgAsInteger(0),
    GetInputArgAsInteger(1),GetInputArgAsInteger(2)));
end;

procedure TGridScript.ColumnMin;
begin
  ReturnOutputArg(TAdvStringGrid(CurrentObject).ColumnMin(GetInputArgAsInteger(0),
    GetInputArgAsInteger(1),GetInputArgAsInteger(2)));
end;

procedure TGridScript.ColumnSum;
begin
  ReturnOutputArg(TAdvStringGrid(CurrentObject).ColumnSum(GetInputArgAsInteger(0),
    GetInputArgAsInteger(1),GetInputArgAsInteger(2)));
end;

procedure TGridScript.RowAvg;
begin
  ReturnOutputArg(TAdvStringGrid(CurrentObject).RowAvg(GetInputArgAsInteger(0),
    GetInputArgAsInteger(1),GetInputArgAsInteger(2)));
end;

procedure TGridScript.RowMax;
begin
  ReturnOutputArg(TAdvStringGrid(CurrentObject).RowMax(GetInputArgAsInteger(0),
    GetInputArgAsInteger(1),GetInputArgAsInteger(2)));
end;

procedure TGridScript.RowMin;
begin
  ReturnOutputArg(TAdvStringGrid(CurrentObject).RowMin(GetInputArgAsInteger(0),
    GetInputArgAsInteger(1),GetInputArgAsInteger(2)));
end;

procedure TGridScript.RowSum;
begin
  ReturnOutputArg(TAdvStringGrid(CurrentObject).RowSum(GetInputArgAsInteger(0),
    GetInputArgAsInteger(1),GetInputArgAsInteger(2)));
end;

procedure TGridScript.AddComboString;
begin
  TAdvStringGrid(CurrentObject).AddComboString(GetInputArgAsString(0));
end;

procedure TGridScript.AddComboStringObject;
begin
  TAdvStringGrid(CurrentObject).AddComboStringObject(GetInputArgAsString(0),VarToObject(GetInputArg(1)));
end;

procedure TGridScript.ClearComboString;
begin
  TAdvStringGrid(CurrentObject).ClearComboString;
end;

procedure TGridScript.GetComboCount;
begin
  ReturnOutputArg(TAdvStringGrid(CurrentObject).GetComboCount);
end;

procedure TGridScript.GetVersionNr;
begin
  ReturnOutputArg(TAdvStringGrid(CurrentObject).GetVersionNr);
end;

procedure TGridScript.GetVersionString;
begin
  ReturnOutputArg(TAdvStringGrid(CurrentObject).GetVersionString);
end;

procedure TGridScript.RemoveComboString;
begin
  ReturnOutputArg(TAdvStringGrid(CurrentObject).RemoveComboString(GetInputArgAsString(0)));
end;

procedure TGridScript.SetComboSelection;
begin
  TAdvStringGrid(CurrentObject).SetComboSelection(GetInputArgAsInteger(0));
end;

procedure TGridScript.SetComboSelectionString;
begin
  ReturnOutputArg(TAdvStringGrid(CurrentObject).SetComboSelectionString(GetInputArgAsString(0)));
end;

procedure TGridScript.HideInplaceEdit;
begin
  TAdvStringGrid(CurrentObject).HideInplaceEdit;
end;

procedure TGridScript.HideSelection;
begin
  TAdvStringGrid(CurrentObject).HideSelection;
end;

procedure TGridScript.IsSelected;
begin
  ReturnOutputArg(TAdvStringGrid(CurrentObject).IsSelected(GetInputArgAsInteger(0),GetInputArgAsInteger(1)));
end;

procedure TGridScript.ScrollInView;
begin
  TAdvStringGrid(CurrentObject).ScrollInView(GetInputArgAsInteger(0),GetInputArgAsInteger(1));
end;

procedure TGridScript.ShowInplaceEdit;
begin
  TAdvStringGrid(CurrentObject).ShowInplaceEdit;
end;

procedure TGridScript.UnHideSelection;
begin
  TAdvStringGrid(CurrentObject).UnHideSelection;
end;

procedure TGridScript.LoadFromFixed;
begin
  TAdvStringGrid(CurrentObject).LoadFromFixed(GetInputArgAsString(0),TIntList(VarToObject(GetInputArg(1))));
end;

procedure TGridScript.LoadFromStream;
begin
  TAdvStringGrid(CurrentObject).LoadFromStream(TStream(VarToObject(GetInputArg(0))));
end;

procedure TGridScript.SaveToFixed;
begin
  TAdvStringGrid(CurrentObject).SaveToFixed(GetInputArgAsString(0),TIntList(VarToObject(GetInputArg(1))));
end;

procedure TGridScript.SaveToStream;
begin
  TAdvStringGrid(CurrentObject).SaveToStream(TStream(VarToObject(GetInputArg(0))));
end;

procedure TGridScript.SaveToXML;
begin
  TAdvStringGrid(CurrentObject).SaveToXML(GetInputArgAsString(0),GetInputArgAsString(1),
                                          GetInputArgAsString(2),TStrings(VarToObject(GetInputArg(3))));
end;

procedure TGridScript.IsHiddenColumn;
begin
  ReturnOutputArg(TAdvStringGrid(CurrentObject).IsHiddenColumn(GetInputArgAsInteger(0)));
end;

procedure TGridScript.IsHiddenRow;
begin
  ReturnOutputArg(TAdvStringGrid(CurrentObject).IsHiddenRow(GetInputArgAsInteger(0)));
end;

procedure TGridScript.NumHiddenColumns;
begin
  ReturnOutputArg(TAdvStringGrid(CurrentObject).NumHiddenColumns);
end;

procedure TGridScript.NumHiddenRows;
begin
  ReturnOutputArg(TAdvStringGrid(CurrentObject).NumHiddenRows);
end;

procedure TGridScript.RemoveSelectedRows;
begin
  TAdvStringGrid(CurrentObject).RemoveSelectedRows;
end;

procedure TGridScript.HideSelectedRows;
begin
  TAdvStringGrid(CurrentObject).HideSelectedRows;
end;

procedure TGridScript.AddCheckBox;
begin
  TAdvStringGrid(CurrentObject).AddCheckBox(GetInputArgAsInteger(0),GetInputArgAsInteger(1),
                                            GetInputArgAsBoolean(2),GetInputArgAsBoolean(3));
end;

procedure TGridScript.GetCheckBoxState;
var
  b: boolean;
begin
  TAdvStringGrid(CurrentObject).GetCheckBoxState(GetInputArgAsInteger(0),GetInputArgAsInteger(1),b);
  ReturnOutputArg(b);
end;

procedure TGridScript.HasCheckBox;
begin
  ReturnOutputArg(TAdvStringGrid(CurrentObject).HasCheckBox(GetInputArgAsInteger(0),GetInputArgAsInteger(1)));
end;

procedure TGridScript.HasDataCheckBox;
begin
  ReturnOutputArg(TAdvStringGrid(CurrentObject).HasDataCheckBox(GetInputArgAsInteger(0),GetInputArgAsInteger(1)));
end;

procedure TGridScript.RemoveCheckBox;
begin
  TAdvStringGrid(CurrentObject).RemoveCheckBox(GetInputArgAsInteger(0),GetInputArgAsInteger(1));
end;

procedure TGridScript.SetCheckBoxState;
begin
  TAdvStringGrid(CurrentObject).SetCheckBoxState(GetInputArgAsInteger(0),GetInputArgAsInteger(1),GetInputArgAsBoolean(2));
end;

procedure TGridScript.ToggleCheckBox;
begin
  TAdvStringGrid(CurrentObject).ToggleCheckBox(GetInputArgAsInteger(0),GetInputArgAsInteger(1));
end;

procedure TGridScript.AddComment;
begin
  TAdvStringGrid(CurrentObject).AddComment(GetInputArgAsInteger(0),GetInputArgAsInteger(1),GetInputArgAsString(2));
end;

procedure TGridScript.IsComment;
var
  s:string;
begin
  ReturnOutputArg(TAdvStringGrid(CurrentObject).IsComment(GetInputArgAsInteger(0),GetInputArgAsInteger(1),s));
end;

procedure TGridScript.RemoveComment;
begin
  TAdvStringGrid(CurrentObject).RemoveComment(GetInputArgAsInteger(0),GetInputArgAsInteger(1));
end;

procedure TGridScript.DisplColIndex;
begin
  ReturnOutputArg(TAdvStringGrid(CurrentObject).DisplColIndex(GetInputArgAsInteger(0)));
end;

procedure TGridScript.DisplRowIndex;
begin
  ReturnOutputArg(TAdvStringGrid(CurrentObject).DisplRowIndex(GetInputArgAsInteger(0)));
end;

procedure TGridScript.GetRealCol;
begin
  ReturnOutputArg(TAdvStringGrid(CurrentObject).GetRealCol);
end;

procedure TGridScript.GetRealRow;
begin
  ReturnOutputArg(TAdvStringGrid(CurrentObject).GetRealRow);
end;

procedure TGridScript.RealColIndex;
begin
  ReturnOutputArg(TAdvStringGrid(CurrentObject).RealColIndex(GetInputArgAsInteger(0)));
end;

procedure TGridScript.RealRowIndex;
begin
  ReturnOutputArg(TAdvStringGrid(CurrentObject).RealRowIndex(GetInputArgAsInteger(0)));
end;

procedure TGridScript.FindFirst;
var
  ObjPoint: TPointWrapper;
  APoint: TPoint;
  FParams: TFindParams;

  function ConvertIntegertoFindParams(AInt:integer):TFindParams;
  var
    c: Integer;
  begin
    Result := [];
    for c := 0 to 31 do
    if ((AInt shr c) and 1)=1 then
      Include(Result,TFindParameters(c));
  end;

begin
  ObjPoint := TPointWrapper.Create(Self);
  FParams := ConvertIntegerToFindParams(GetInputArgAsInteger(1));
  APoint := TAdvStringGrid(CurrentObject).FindFirst(GetInputArgAsString(0),FParams);
  ObjPoint.X := APoint.x;
  ObjPoint.Y := APoint.y;
  ReturnOutputArg(Integer(ObjPoint));
end;

procedure TGridScript.FindNext;
var
  ObjPoint: TPointWrapper;
  APoint: TPoint;
begin
  ObjPoint := TPointWrapper.Create(Self);
  APoint := TAdvStringGrid(CurrentObject).FindNext;
  ObjPoint.X := APoint.x;
  ObjPoint.Y := APoint.y;
  ReturnOutputArg(Integer(ObjPoint));
end;


procedure TGridScript.AddImageIdx;
begin
  TAdvStringGrid(CurrentObject).AddImageIdx(GetInputArgAsInteger(0),GetInputArgAsInteger(1),
     GetInputArgAsInteger(2),TCellHAlign(GetInputArgAsInteger(3)),TCellValign(GetInputArgAsInteger(4)));
end;

procedure TGridScript.GetImageIdx;
var
  idx: Integer;
begin
  if TAdvStringGrid(CurrentObject).GetImageIdx(GetInputArgAsInteger(0),GetInputArgAsInteger(1),idx) then
    ReturnOutputArg(idx)
  else
    ReturnOutputArg(-1);
end;

procedure TGridScript.RemoveImageIdx;
begin
  TAdvStringGrid(CurrentObject).RemoveImageIdx(GetInputArgAsInteger(0),GetInputArgAsInteger(1));
end;

procedure TGridScript.AddRotated;
begin
  TAdvStringGrid(CurrentObject).AddRotated(GetInputArgAsInteger(0),GetInputArgAsInteger(1),
    GetInputArgAsInteger(2),GetInputArgAsString(3));
end;

procedure TGridScript.IsRotated;
var
  Angle: Integer;
begin
  if TAdvStringGrid(CurrentObject).IsRotated(GetInputArgAsInteger(0),GetInputArgAsInteger(1),Angle) then
    ReturnOutputArg(Angle)
  else
    ReturnOutputArg(0);
end;

procedure TGridScript.RemoveRotated;
begin
  TAdvStringGrid(CurrentObject).RemoveRotated(GetInputArgAsInteger(0),GetInputArgAsInteger(1));
end;

procedure TGridScript.RepaintCell;
begin
  TAdvStringGrid(CurrentObject).RepaintCell(GetInputArgAsInteger(0),GetInputArgAsInteger(1));
end;

procedure TGridScript.RepaintCol;
begin
  TAdvStringGrid(CurrentObject).RepaintCol(GetInputArgAsInteger(0));
end;

procedure TGridScript.RepaintRect;
var
  r: TRect;
begin
  r.Left := GetInputArgAsInteger(0);
  r.Top := GetInputArgAsInteger(1);
  r.Right := GetInputArgAsInteger(2);
  r.Bottom := GetInputArgAsInteger(3);

  TAdvStringGrid(CurrentObject).RepaintRect(r);
end;

procedure TGridScript.RepaintRow;
begin
  TAdvStringGrid(CurrentObject).RepaintRow(GetInputArgAsInteger(0));
end;

procedure TGridScript._Exclude;
var
  ASet: Integer;
begin
  ASet := GetInputArgAsInteger(0);
  ASet := ASet and not (1 shl GetInputArgAsInteger(1));
  ReturnOutputArg(ASet);
end;

procedure TGridScript._Include;
var
  ASet: Integer;
begin
  ASet := GetInputArgAsInteger(0);
  ASet := ASet or (1 shl GetInputArgAsInteger(1));
  ReturnOutputArg(ASet);
end;

procedure TGridScript.GetDelimiter;
begin
  ReturnOutputArg(TAdvStringGrid(CurrentObject).Delimiter);
end;

procedure TGridScript.SetDelimiter;
var
  s: string;
begin
  s := GetInputArgAsString(0);
  if length(s)>0 then
  TAdvStringGrid(CurrentObject).Delimiter := s[1];
end;

end.
