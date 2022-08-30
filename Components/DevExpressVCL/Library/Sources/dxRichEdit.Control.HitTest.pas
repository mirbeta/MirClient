{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxRichEdit.Control.HitTest;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, SysUtils, Generics.Defaults, Generics.Collections,
  dxCoreClasses, cxClasses,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Boxes.Core,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentLayout.Position;

type
  TdxBoxHitTestManager = class;
  TdxBoxHitTestCalculator = class;
  TdxRichEditHitTestResult = class;

  IdxBoxHitTestCalculator = interface(IdxBoxHitTestCustomCalculator)
  ['{FCB6731A-5045-419E-81C0-995E0D0D9E14}']
    function CreatePageHitTestManager(APage: TdxPage): TdxBoxHitTestManager;
    function CreatePageAreaHitTestManager(APageArea: TdxPageArea): TdxBoxHitTestManager;
    function CreateColumnHitTestManager(AColumn: TdxColumn): TdxBoxHitTestManager;
    function CreateNumberingListBoxHitTestManager(ABox: TdxNumberingListBox): TdxBoxHitTestManager;
    function CreatePageBreakBoxHitTestManager(ABox: TdxPageBreakBox): TdxBoxHitTestManager;
    function CreateColumnBreakBoxHitTestManager(ABox: TdxColumnBreakBox): TdxBoxHitTestManager;
    function CreateSectionMarkBoxHitTestManager(ABox: TdxSectionMarkBox): TdxBoxHitTestManager;
    function CreateTabSpaceBoxHitTestManager(ABox: TdxTabSpaceBox): TdxBoxHitTestManager;
    procedure ProcessPageCollection(ACollection: TdxPageCollection);
    procedure ProcessPageAreaCollection(ACollection: TdxPageAreaCollection);
    procedure ProcessColumnCollection(ACollection: TdxColumnCollection);
    procedure ProcessRowCollection(ACollection: TdxRowCollection);
    procedure ProcessBoxCollection(ACollection: TdxBoxCollection);
  end;

  { TdxRichEditHitTestRequest }

  TdxRichEditHitTestRequest = record
    Accuracy: TdxHitTestAccuracy;
    DetailsLevel: TdxDocumentLayoutDetailsLevel;
    LogicalPoint: TPoint;
    IgnoreInvalidAreas: Boolean;
    PhysicalPoint: TPoint;
    PieceTable: TdxPieceTable;
    SearchAnyPieceTable: Boolean;
    constructor Create(APieceTable: TdxPieceTable);
    function Clone: TdxRichEditHitTestRequest;
  end;

  TdxCommentLocationType = (
    CommentContent,
    CommentMoreButton,
    None
  );

  { TdxRichEditHitTestResult }

  TdxRichEditHitTestResult = class(TdxDocumentLayoutPosition)
  private
    FAccuracy: TdxHitTestAccuracy;
    FLogicalPoint: TPoint;
    FPhysicalPoint: TPoint;
    FFloatingObjectBox: TdxFloatingObjectBox;
    FCommentViewInfo: TdxCommentViewInfo;
    FCommentLocation: TdxCommentLocationType;
  public
    constructor Create(ADocumentLayout: TdxDocumentLayout; APieceTable: TdxPieceTable); reintroduce;

    procedure CopyFrom(Source: TdxCloneable); override;

    property Accuracy: TdxHitTestAccuracy read FAccuracy write FAccuracy;
    property LogicalPoint: TPoint read FLogicalPoint write FLogicalPoint;
    property PhysicalPoint: TPoint read FPhysicalPoint write FPhysicalPoint;
    property FloatingObjectBox: TdxFloatingObjectBox read FFloatingObjectBox write FFloatingObjectBox;
    property CommentViewInfo: TdxCommentViewInfo read FCommentViewInfo write FCommentViewInfo;
    property CommentLocation: TdxCommentLocationType read FCommentLocation write FCommentLocation;
  end;

  { TdxBoxHitTestCalculator }

  TdxBoxHitTestCalculator = class(TdxBoxHitTestCustomCalculator, IdxBoxHitTestCalculator)
  private
    FHitTestRequest: TdxRichEditHitTestRequest;
    FHitTestResult: TdxRichEditHitTestResult;
  protected
    procedure CalcHitTestCore(AManager: TdxBoxHitTestManager; AForceStrictHitTest: Boolean);
    function FastHitTestCore(ACollection: TdxCustomBoxCollection;
      APredicate: TdxBoxComparable; AStrictHitTest: Boolean): Integer;
    procedure RegisterBoxHitTest(ABox: TdxBox); override;
    procedure RegisterCharacterHitTest(ABox: TdxCharacterBox); override;
    //IdxBoxHitTestCalculator
    function CreatePageHitTestManager(APage: TdxPage): TdxBoxHitTestManager;
    function CreatePageAreaHitTestManager(APageArea: TdxPageArea): TdxBoxHitTestManager;
    function CreateColumnHitTestManager(AColumn: TdxColumn): TdxBoxHitTestManager;
    function CreateTabSpaceBoxHitTestManager(ABox: TdxTabSpaceBox): TdxBoxHitTestManager;
    function CreateNumberingListBoxHitTestManager(ABox: TdxNumberingListBox): TdxBoxHitTestManager;
    function CreatePageBreakBoxHitTestManager(ABox: TdxPageBreakBox): TdxBoxHitTestManager;
    function CreateColumnBreakBoxHitTestManager(ABox: TdxColumnBreakBox): TdxBoxHitTestManager;
    function CreateSectionMarkBoxHitTestManager(ABox: TdxSectionMarkBox): TdxBoxHitTestManager;
    procedure ProcessPageCollection(ACollection: TdxPageCollection); virtual;
    procedure ProcessPageAreaCollection(ACollection: TdxPageAreaCollection); virtual;
    procedure ProcessColumnCollection(ACollection: TdxColumnCollection); virtual;
    procedure ProcessRowCollection(ACollection: TdxRowCollection); virtual;
  public
    constructor Create(const ARequest: TdxRichEditHitTestRequest; AResult: TdxRichEditHitTestResult);

  {$REGION 'IdxBoxHitTestCustomCalculator'}
    function CreateCharacterBoxHitTestManager(ABox: TdxCharacterBox): TdxBoxHitTestCustomManager; override;
    function CreateHyphenBoxHitTestManager(ABox: TdxHyphenBox): TdxBoxHitTestCustomManager; override;
    function CreateRowHitTestManager(ARow: TdxSimpleRow): TdxBoxHitTestCustomManager; override;
    function CreateTextBoxHitTestManager(ABox: TdxTextBox): TdxBoxHitTestCustomManager; override;
    function CreateInlinePictureBoxHitTestManager(ABox: TdxInlinePictureBox): TdxBoxHitTestCustomManager; override;
    function CreateSeparatorMarkBoxHitTestManager(ABox: TdxSeparatorBox): TdxBoxHitTestCustomManager; override;
    function CreateSpaceBoxHitTestManager(const ABox: IdxSpaceBox): TdxBoxHitTestCustomManager; override;
    function CreateLineBreakBoxHitTestManager(ABox: TdxLineBreakBox): TdxBoxHitTestCustomManager; override;
    function CreateParagraphMarkBoxHitTestManager(ABox: TdxParagraphMarkBox): TdxBoxHitTestCustomManager; override;
    procedure ProcessBoxCollection(ACollection: TdxBoxCollection); override;
    procedure ProcessCharacterBoxCollection(ACollection: TdxCharacterBoxCollection); override;
  {$ENDREGION}

    procedure CalcHitTest(ABox: TdxBox; AForceStrictHitTest: Boolean = False);

    procedure FastHitTestCharacter(ACollection: TdxCharacterBoxCollection; AStrictHitTest: Boolean);
    function FastHitTestAssumingArrangedHorizontally(ACollection: TdxCustomBoxCollection; AStrictHitTest: Boolean): Integer;
    function FastHitTestAssumingArrangedVertically(ACollection: TdxCustomBoxCollection; AStrictHitTest: Boolean): Integer;

    function FastHitTestIndexCore(ACollection: TdxBoxList;
      APredicate: TdxBoxComparable; AStrictHitTest: Boolean): Integer; overload;
    function FastHitTestIndexCore<T: class>(ACollection: TdxList<T>;
      const APredicate: IdxComparable<T>; AStrictHitTest: Boolean): Integer; overload;
    function FastHitTestIndexCore<T: class>(ACollection: TdxList<T>; AStartIndex, ACount: Integer;
      const APredicate: IdxComparable<T>; AStrictHitTest: Boolean): Integer; overload;
    function FastHitTestRowIndex(ARows: TdxRowCollection): Integer;

    property HitTestRequest: TdxRichEditHitTestRequest read FHitTestRequest;
    property HitTestResult: TdxRichEditHitTestResult read FHitTestResult;
  end;

  { TdxBoxHitTestManager }

  TdxBoxHitTestManager = class(TdxBoxHitTestCustomManager)
  private
    function GetCalculator: TdxBoxHitTestCalculator;
    function GetHitTestRequest: TdxRichEditHitTestRequest;
    function GetHitTestResult: TdxRichEditHitTestResult;
    function GetICalculator: IdxBoxHitTestCalculator;
  protected
    property ICalculator: IdxBoxHitTestCalculator read GetICalculator;
  public
    procedure CalcHitTest(AForceStrictHitTest: Boolean); virtual;
    procedure CalcHitTestAmongNestedBoxes; virtual;
    procedure RegisterSuccessfullHitTest(AStrictMatch: Boolean); virtual;
    procedure RegisterFailedHitTest; virtual;

		property Calculator: TdxBoxHitTestCalculator read GetCalculator;
    property HitTestRequest: TdxRichEditHitTestRequest read GetHitTestRequest;
    property HitTestResult: TdxRichEditHitTestResult read GetHitTestResult;
  end;

  { TdxPageHitTestManager }

  TdxPageHitTestManager = class(TdxBoxHitTestManager)
  private
    function GetPage: TdxPage;
  public
    procedure CalcHitTestAmongNestedBoxes; override;
    procedure RegisterSuccessfullHitTest(AStrictMatch: Boolean); override;
    procedure RegisterFailedHitTest; override;
    property Page: TdxPage read GetPage;
  end;

  { TdxPageAreaHitTestManager }

  TdxPageAreaHitTestManager = class(TdxBoxHitTestManager)
  public type
    TPageAreaKind = (
      Autodetect = 0,
      Main,
      Header,
      Footer,
      TextBox
    );
  strict private
    FPageAreaKind: TPageAreaKind;
    function AutodetectPageAreaKind: TPageAreaKind;
    function GetPageArea: TdxPageArea;
    function GetAreaKind: TPageAreaKind;
    function ShouldIgnoreFailedHitTest: Boolean;
  public
    procedure CalcHitTest(AForceStrictHitTest: Boolean); override;
    procedure CalcHitTestAmongNestedBoxes; override;
    procedure RegisterSuccessfullHitTest(AStrictMatch: Boolean); override;
    procedure RegisterFailedHitTest; override;

    property AreaKind: TPageAreaKind read GetAreaKind write FPageAreaKind;
    property PageArea: TdxPageArea read GetPageArea;
  end;

  { TdxColumnHitTestManager }

  TdxColumnHitTestManager = class(TdxBoxHitTestManager)
  private
    function GetColumn: TdxColumn;
    function GetTable(ATableCellRow: TdxTableCellRow): TdxTableViewInfo;
  protected
    function CalcHitTestAmongNestedBoxesCore(ATable: TdxTableViewInfo; AStrictTableMatch: Boolean): Boolean; overload;
    function CalcHitTestAmongNestedBoxesCore(ATable: TdxTableViewInfo; AStrictTableMatch, AStrictTableRowMatch, AStrictTableCellMatch: Boolean): Boolean; overload;
    function GetActiveTableViewInfo: TdxTableViewInfo;
  public
    procedure CalcHitTestAmongNestedBoxes; override;
    procedure RegisterSuccessfullHitTest(AStrictMatch: Boolean); override;
    procedure RegisterFailedHitTest; override;
    property Column: TdxColumn read GetColumn;
  end;

  { TdxRowHitTestManager }

  TdxRowHitTestManager = class(TdxBoxHitTestManager)
  private
    function GetRow: TdxRow;
  public
    procedure CalcHitTestAmongNestedBoxes; override;
    procedure RegisterSuccessfullHitTest(AStrictMatch: Boolean); override;
    procedure RegisterFailedHitTest; override;
    property Row: TdxRow read GetRow;
  end;

  { TdxCharacterBoxHitTestManager }

  TdxCharacterBoxHitTestManager = class(TdxBoxHitTestManager)
  private
    function GetCharacter: TdxCharacterBox;
  public
    procedure CalcHitTestAmongNestedBoxes; override;
    procedure RegisterSuccessfullHitTest(AStrictMatch: Boolean); override;
    procedure RegisterFailedHitTest; override;
    property Character: TdxCharacterBox read GetCharacter;
  end;

  { TdxEmptyHitTestManager }

  TdxEmptyHitTestManager = class(TdxBoxHitTestManager)
  public
    procedure CalcHitTestAmongNestedBoxes; override;
    procedure RegisterSuccessfullHitTest(AStrictMatch: Boolean); override;
    procedure RegisterFailedHitTest; override;
  end;

implementation

uses
  dxTypeHelpers,
  dxRichEdit.Utils.Exceptions;

{ TdxRichEditHitTestRequest }

constructor TdxRichEditHitTestRequest.Create(APieceTable: TdxPieceTable);
begin
  FillChar(Self, SizeOf(TdxRichEditHitTestRequest), 0);
  PieceTable := APieceTable;
end;

function TdxRichEditHitTestRequest.Clone: TdxRichEditHitTestRequest;
begin
  Result.PieceTable := PieceTable;

  Result.PhysicalPoint := PhysicalPoint;
  Result.LogicalPoint := LogicalPoint;
  Result.DetailsLevel := DetailsLevel;
  Result.Accuracy := Accuracy;
  Result.SearchAnyPieceTable := SearchAnyPieceTable;
  Result.IgnoreInvalidAreas := False;
end;

{ TdxRichEditHitTestResult }

constructor TdxRichEditHitTestResult.Create(ADocumentLayout: TdxDocumentLayout; APieceTable: TdxPieceTable);
begin
  inherited Create(ADocumentLayout, APieceTable, 0);
end;

procedure TdxRichEditHitTestResult.CopyFrom(Source: TdxCloneable);
var
  ASource: TdxRichEditHitTestResult absolute Source;
begin
  inherited CopyFrom(ASource);
  Accuracy := ASource.Accuracy;
  LogicalPoint := ASource.LogicalPoint;
  PhysicalPoint := ASource.PhysicalPoint;
end;

{ TdxBoxHitTestCalculator }

constructor TdxBoxHitTestCalculator.Create(const ARequest: TdxRichEditHitTestRequest; AResult: TdxRichEditHitTestResult);
begin
  inherited Create;
  FHitTestRequest := ARequest;
  FHitTestResult := AResult;
  FHitTestResult.LogicalPoint := FHitTestRequest.LogicalPoint;
end;

procedure TdxBoxHitTestCalculator.CalcHitTest(ABox: TdxBox; AForceStrictHitTest: Boolean = False);
var
  AManager: TdxBoxHitTestManager;
begin
  AManager := TdxBoxHitTestManager(ABox.CreateHitTestManager(Self));
  try
    AManager.CalcHitTest(AForceStrictHitTest);
  finally
    AManager.Free;
  end;
end;

procedure TdxBoxHitTestCalculator.CalcHitTestCore(AManager: TdxBoxHitTestManager; AForceStrictHitTest: Boolean);
var
  AStrictHitTest, AStrictMatch: Boolean;
begin
  AStrictHitTest := ((Ord(HitTestRequest.Accuracy) and Ord(AManager.Box.HitTestAccuracy)) <> 0) or AForceStrictHitTest;
  AStrictMatch := AManager.Box.Bounds.Contains(HitTestRequest.LogicalPoint);
  if not AStrictHitTest and (HitTestRequest.DetailsLevel >= AManager.Box.DetailsLevel) then
  begin
    AManager.RegisterSuccessfullHitTest(AStrictMatch);
    if HitTestRequest.DetailsLevel > AManager.Box.DetailsLevel then
      AManager.CalcHitTestAmongNestedBoxes;
  end
  else
    if AStrictMatch then
    begin
      AManager.RegisterSuccessfullHitTest(AStrictMatch);
      if HitTestRequest.DetailsLevel > AManager.Box.DetailsLevel then
        AManager.CalcHitTestAmongNestedBoxes;
    end
    else
      AManager.RegisterFailedHitTest;
end;

function TdxBoxHitTestCalculator.FastHitTestIndexCore(ACollection: TdxBoxList;
  APredicate: TdxBoxComparable; AStrictHitTest: Boolean): Integer;
begin
  Result := FastHitTestIndexCore<TdxBoxBase>(ACollection, APredicate, AStrictHitTest);
end;

function TdxBoxHitTestCalculator.FastHitTestIndexCore<T>(ACollection: TdxList<T>; AStartIndex, ACount: Integer;
  const APredicate: IdxComparable<T>; AStrictHitTest: Boolean): Integer;
begin
  if ACollection.Count <= 0 then
    Exit(-1);

  if TdxAlgorithms1<T>.BinarySearch(ACollection, APredicate, AStartIndex, ACount - 1, Result) then
    Exit;

  if AStrictHitTest then
    Exit(-1);

  if Result = ACollection.Count then
  begin
    if APredicate.CompareTo(ACollection[0]) > 0 then
      Result := 0
    else
      Result := ACollection.Count - 1;
  end;
end;

function TdxBoxHitTestCalculator.FastHitTestIndexCore<T>(ACollection: TdxList<T>;
  const APredicate: IdxComparable<T>; AStrictHitTest: Boolean): Integer;
begin
  Result := FastHitTestIndexCore<T>(ACollection, 0, ACollection.Count, APredicate, AStrictHitTest);
end;

function TdxBoxHitTestCalculator.FastHitTestRowIndex(ARows: TdxRowCollection): Integer;
var
  AStrictHitTest: Boolean;
  AComparable: TdxBoxAndPointYComparable;
begin
  AStrictHitTest := (HitTestRequest.Accuracy and ExactRow) <> 0;
  AComparable := TdxBoxAndPointYComparable.Create(HitTestRequest.LogicalPoint);
  try
    Result := FastHitTestIndexCore(ARows, AComparable, AStrictHitTest);
  finally
    AComparable.Free;
  end;
end;

function TdxBoxHitTestCalculator.FastHitTestCore(ACollection: TdxCustomBoxCollection;
  APredicate: TdxBoxComparable; AStrictHitTest: Boolean): Integer;
begin
  Result := FastHitTestIndexCore(ACollection, APredicate, AStrictHitTest);
  if Result >= 0 then
  begin
    CalcHitTest(TdxBox(ACollection[Result]));
    ACollection.RegisterSuccessfullItemHitTest(Self, TdxBox(ACollection[Result]));
  end
  else
    ACollection.RegisterFailedItemHitTest(Self);
end;

procedure TdxBoxHitTestCalculator.RegisterBoxHitTest(ABox: TdxBox);
begin
  HitTestResult.Box := ABox;
  if ABox <> nil then
    HitTestResult.IncreaseDetailsLevel(TdxDocumentLayoutDetailsLevel.Box);
end;

procedure TdxBoxHitTestCalculator.RegisterCharacterHitTest(ABox: TdxCharacterBox);
begin
  HitTestResult.Character := ABox;
  if ABox <> nil then
    HitTestResult.IncreaseDetailsLevel(TdxDocumentLayoutDetailsLevel.Character);
end;

procedure TdxBoxHitTestCalculator.FastHitTestCharacter(ACollection: TdxCharacterBoxCollection; AStrictHitTest: Boolean);
var
  ACharacterIndex: Integer;
  ABounds: TRect;
  APoint: TPoint;
  ALeftDistance, ARightDistance: Integer;
begin
  ACharacterIndex := FastHitTestAssumingArrangedHorizontally(ACollection, AStrictHitTest);
  if not AStrictHitTest and (HitTestResult.Character <> nil) and (ACharacterIndex >= 0) then
  begin
    ABounds := HitTestResult.Character.Bounds;
    APoint := HitTestRequest.LogicalPoint;
    if (ABounds.Left <= APoint.X) and (APoint.X <= ABounds.Right) then
    begin
      ALeftDistance := APoint.X - ABounds.Left;
      ARightDistance := ABounds.Right - APoint.X;
      if ALeftDistance >= ARightDistance then
      begin
        if ACharacterIndex + 1 < ACollection.Count then
        begin
          HitTestResult.Character := ACollection[ACharacterIndex + 1];
          HitTestResult.Accuracy := HitTestResult.Accuracy and not ExactCharacter;
        end;
      end;
    end;
  end;
end;

function TdxBoxHitTestCalculator.FastHitTestAssumingArrangedHorizontally(
  ACollection: TdxCustomBoxCollection; AStrictHitTest: Boolean): Integer;
var
  AComparer: TdxBoxAndPointXComparable;
begin
  AComparer := TdxBoxAndPointXComparable.Create(HitTestRequest.LogicalPoint);
  try
    Result := FastHitTestCore(ACollection, AComparer, AStrictHitTest);
  finally
    AComparer.Free;
  end;
end;

function TdxBoxHitTestCalculator.FastHitTestAssumingArrangedVertically(
  ACollection: TdxCustomBoxCollection; AStrictHitTest: Boolean): Integer;
var
  AComparer: TdxBoxAndPointYComparable;
begin
  AComparer := TdxBoxAndPointYComparable.Create(HitTestRequest.LogicalPoint);
  try
    Result := FastHitTestCore(ACollection, AComparer, AStrictHitTest);
  finally
    AComparer.Free;
  end;
end;

function TdxBoxHitTestCalculator.CreatePageHitTestManager(APage: TdxPage): TdxBoxHitTestManager;
begin
  Result := TdxPageHitTestManager.Create(Self, APage);
end;

function TdxBoxHitTestCalculator.CreatePageAreaHitTestManager(APageArea: TdxPageArea): TdxBoxHitTestManager;
begin
  Result := TdxPageAreaHitTestManager.Create(Self, APageArea);
end;

function TdxBoxHitTestCalculator.CreateColumnHitTestManager(AColumn: TdxColumn): TdxBoxHitTestManager;
begin
  Result := TdxColumnHitTestManager.Create(Self, AColumn);
end;

function TdxBoxHitTestCalculator.CreateRowHitTestManager(ARow: TdxSimpleRow): TdxBoxHitTestCustomManager;
begin
  Result := TdxRowHitTestManager.Create(Self, ARow);
end;

function TdxBoxHitTestCalculator.CreateTextBoxHitTestManager(ABox: TdxTextBox): TdxBoxHitTestCustomManager;
begin
  Result := TdxBoxHitTestManager.Create(Self, ABox);
end;

function TdxBoxHitTestCalculator.CreateCharacterBoxHitTestManager(ABox: TdxCharacterBox): TdxBoxHitTestCustomManager;
begin
  Result := TdxCharacterBoxHitTestManager.Create(Self, ABox);
end;

function TdxBoxHitTestCalculator.CreateHyphenBoxHitTestManager(ABox: TdxHyphenBox): TdxBoxHitTestCustomManager;
begin
  Result := TdxBoxHitTestManager.Create(Self, ABox);
end;

function TdxBoxHitTestCalculator.CreateInlinePictureBoxHitTestManager(ABox: TdxInlinePictureBox): TdxBoxHitTestCustomManager;
begin
  Result := TdxBoxHitTestManager.Create(Self, ABox);
end;

function TdxBoxHitTestCalculator.CreateSeparatorMarkBoxHitTestManager(ABox: TdxSeparatorBox): TdxBoxHitTestCustomManager;
begin
  Result := TdxBoxHitTestManager.Create(Self, ABox);
end;

function TdxBoxHitTestCalculator.CreateSpaceBoxHitTestManager(const ABox: IdxSpaceBox): TdxBoxHitTestCustomManager;
begin
  Result := TdxBoxHitTestManager.Create(Self, ABox.Box);
end;

function TdxBoxHitTestCalculator.CreateTabSpaceBoxHitTestManager(ABox: TdxTabSpaceBox): TdxBoxHitTestManager;
begin
  Result := TdxBoxHitTestManager.Create(Self, ABox);
end;

function TdxBoxHitTestCalculator.CreateNumberingListBoxHitTestManager(ABox: TdxNumberingListBox): TdxBoxHitTestManager;
begin
  Result := TdxBoxHitTestManager.Create(Self, ABox);
end;

function TdxBoxHitTestCalculator.CreateLineBreakBoxHitTestManager(ABox: TdxLineBreakBox): TdxBoxHitTestCustomManager;
begin
  Result := TdxBoxHitTestManager.Create(Self, ABox);
end;

function TdxBoxHitTestCalculator.CreatePageBreakBoxHitTestManager(ABox: TdxPageBreakBox): TdxBoxHitTestManager;
begin
  Result := TdxBoxHitTestManager.Create(Self, ABox);
end;

function TdxBoxHitTestCalculator.CreateColumnBreakBoxHitTestManager(ABox: TdxColumnBreakBox): TdxBoxHitTestManager;
begin
  Result := TdxBoxHitTestManager.Create(Self, ABox);
end;

function TdxBoxHitTestCalculator.CreateParagraphMarkBoxHitTestManager(ABox: TdxParagraphMarkBox): TdxBoxHitTestCustomManager;
begin
  Result := TdxBoxHitTestManager.Create(Self, ABox);
end;

function TdxBoxHitTestCalculator.CreateSectionMarkBoxHitTestManager(ABox: TdxSectionMarkBox): TdxBoxHitTestManager;
begin
  Result := TdxBoxHitTestManager.Create(Self, ABox);
end;

procedure TdxBoxHitTestCalculator.ProcessPageCollection(ACollection: TdxPageCollection);
begin
//do nothing
end;

procedure TdxBoxHitTestCalculator.ProcessPageAreaCollection(ACollection: TdxPageAreaCollection);
var
  AStrictHitTest: Boolean;
begin
  AStrictHitTest := (HitTestRequest.Accuracy and ExactPageArea) <> 0;
  FastHitTestAssumingArrangedVertically(ACollection, AStrictHitTest);
end;

procedure TdxBoxHitTestCalculator.ProcessColumnCollection(ACollection: TdxColumnCollection);
var
  AStrictHitTest: Boolean;
begin
  AStrictHitTest := (HitTestRequest.Accuracy and ExactColumn) <> 0;
  FastHitTestAssumingArrangedHorizontally(ACollection, AStrictHitTest);
end;

procedure TdxBoxHitTestCalculator.ProcessRowCollection(ACollection: TdxRowCollection);
var
  AStrictHitTest: Boolean;
begin
  AStrictHitTest := (HitTestRequest.Accuracy and ExactRow) <> 0;
  FastHitTestAssumingArrangedVertically(ACollection, AStrictHitTest);
end;

procedure TdxBoxHitTestCalculator.ProcessBoxCollection(ACollection: TdxBoxCollection);
var
  AStrictHitTest: Boolean;
begin
  AStrictHitTest := (HitTestRequest.Accuracy and ExactBox) <> 0;
  FastHitTestAssumingArrangedHorizontally(ACollection, AStrictHitTest);
end;

procedure TdxBoxHitTestCalculator.ProcessCharacterBoxCollection(ACollection: TdxCharacterBoxCollection);
var
  AStrictHitTest: Boolean;
begin
  AStrictHitTest := (HitTestRequest.Accuracy and ExactCharacter) <> 0;
  FastHitTestAssumingArrangedHorizontally(ACollection, AStrictHitTest);
end;

{ TdxBoxHitTestManager }

procedure TdxBoxHitTestManager.CalcHitTest(AForceStrictHitTest: Boolean);
var
  AStrictHitTest, AStrictMatch: Boolean;
begin
  AStrictHitTest := ((HitTestRequest.Accuracy and Box.HitTestAccuracy) <> 0) or AForceStrictHitTest;
  AStrictMatch := Box.Bounds.Contains(HitTestRequest.LogicalPoint);
  if not AStrictHitTest and (HitTestRequest.DetailsLevel >= Box.DetailsLevel) then
  begin
    RegisterSuccessfullHitTest(AStrictMatch);
    if HitTestRequest.DetailsLevel > Box.DetailsLevel then
      CalcHitTestAmongNestedBoxes;
    Exit;
  end;

  if AStrictMatch then
  begin
    RegisterSuccessfullHitTest(AStrictMatch);
    if HitTestRequest.DetailsLevel > Box.DetailsLevel then
      CalcHitTestAmongNestedBoxes;
  end
  else
    RegisterFailedHitTest;
end;

procedure TdxBoxHitTestManager.CalcHitTestAmongNestedBoxes;
begin
//do nothing
end;

procedure TdxBoxHitTestManager.RegisterSuccessfullHitTest(AStrictMatch: Boolean);
begin
  HitTestResult.Box := TdxBox(Box);
  HitTestResult.IncreaseDetailsLevel(TdxDocumentLayoutDetailsLevel.Box);
  if AStrictMatch then
    HitTestResult.Accuracy := HitTestResult.Accuracy or ExactBox;
end;

procedure TdxBoxHitTestManager.RegisterFailedHitTest;
begin
  HitTestResult.Row := nil;
end;

function TdxBoxHitTestManager.GetCalculator: TdxBoxHitTestCalculator;
begin
  Result := TdxBoxHitTestCalculator(inherited Calculator);
end;

function TdxBoxHitTestManager.GetHitTestRequest: TdxRichEditHitTestRequest;
begin
  Result := Calculator.HitTestRequest;
end;

function TdxBoxHitTestManager.GetHitTestResult: TdxRichEditHitTestResult;
begin
  Result := Calculator.HitTestResult;
end;

function TdxBoxHitTestManager.GetICalculator: IdxBoxHitTestCalculator;
begin
  Result := Calculator;
end;

{ TdxPageHitTestManager }

procedure TdxPageHitTestManager.CalcHitTestAmongNestedBoxes;
begin
  ICalculator.ProcessPageAreaCollection(Page.Areas);
end;

procedure TdxPageHitTestManager.RegisterSuccessfullHitTest(AStrictMatch: Boolean);
begin
  HitTestResult.Page := Page;
  HitTestResult.IncreaseDetailsLevel(TdxDocumentLayoutDetailsLevel.Page);
  if AStrictMatch then
    HitTestResult.Accuracy := HitTestResult.Accuracy or ExactPage;
end;

procedure TdxPageHitTestManager.RegisterFailedHitTest;
begin
  HitTestResult.Page := nil;
end;

function TdxPageHitTestManager.GetPage: TdxPage;
begin
  Result := TdxPage(Box)
end;

{ TdxPageAreaHitTestManager }

function TdxPageAreaHitTestManager.GetPageArea: TdxPageArea;
begin
  Result := TdxPageArea(Box)
end;

function TdxPageAreaHitTestManager.AutodetectPageAreaKind: TPageAreaKind;
begin
  if HitTestRequest.PieceTable.IsMain then
    Exit(TPageAreaKind.Main);
  if HitTestRequest.PieceTable.IsHeader then
    Exit(TPageAreaKind.Header);
  if HitTestRequest.PieceTable.IsFooter then
    Exit(TPageAreaKind.Footer);
  if HitTestRequest.PieceTable.IsTextBox then
    Exit(TPageAreaKind.TextBox);
  TdxRichEditExceptions.ThrowInternalException;
  Exit(TPageAreaKind.Autodetect);
end;

procedure TdxPageAreaHitTestManager.CalcHitTestAmongNestedBoxes;
begin
  ICalculator.ProcessColumnCollection(PageArea.Columns);
end;

procedure TdxPageAreaHitTestManager.RegisterSuccessfullHitTest(AStrictMatch: Boolean);
begin
  HitTestResult.PageArea := PageArea;
  HitTestResult.IncreaseDetailsLevel(TdxDocumentLayoutDetailsLevel.PageArea);
  if AStrictMatch then
    HitTestResult.Accuracy := HitTestResult.Accuracy or ExactPageArea;
end;

procedure TdxPageAreaHitTestManager.RegisterFailedHitTest;
begin
  HitTestResult.PageArea := nil;
end;

procedure TdxPageAreaHitTestManager.CalcHitTest(AForceStrictHitTest: Boolean);
begin
  if not AForceStrictHitTest then
  begin
    if not HitTestRequest.IgnoreInvalidAreas and not ShouldIgnoreFailedHitTest then
    begin
      case AreaKind of
        TPageAreaKind.Main:
          if ((HitTestResult.Page.Header <> nil) and (HitTestRequest.LogicalPoint.Y < PageArea.Bounds.Top)) or
             ((HitTestResult.Page.Footer <> nil) and (HitTestRequest.LogicalPoint.Y > PageArea.Bounds.Bottom)) then
          begin
            RegisterFailedHitTest;
            Exit;
          end;
        TPageAreaKind.Header:
          if HitTestRequest.LogicalPoint.Y > PageArea.Bounds.Bottom then
          begin
            RegisterFailedHitTest;
            Exit;
          end;
        TPageAreaKind.Footer:
          if HitTestRequest.LogicalPoint.Y < PageArea.Bounds.Top then
          begin
            RegisterFailedHitTest;
            Exit;
          end;
        TPageAreaKind.TextBox:
          if not PageArea.Bounds.Contains(HitTestRequest.LogicalPoint) then
          begin
            RegisterFailedHitTest;
            Exit;
          end;
      end;
    end;
    RegisterSuccessfullHitTest(True);
  end;
  inherited CalcHitTest(AForceStrictHitTest);
end;

function TdxPageAreaHitTestManager.ShouldIgnoreFailedHitTest: Boolean;
var
  AFloatingObjectBox: TdxFloatingObjectBox;
begin
  AFloatingObjectBox := HitTestResult.FloatingObjectBox;
  if AFloatingObjectBox = nil then
    Exit(False);
  Result := AFloatingObjectBox.GetFloatingObjectRun.PieceTable = HitTestRequest.PieceTable;
end;

function TdxPageAreaHitTestManager.GetAreaKind: TPageAreaKind;
begin
  if FPageAreaKind = TdxPageAreaHitTestManager.TPageAreaKind.Autodetect then
    Result := AutodetectPageAreaKind
  else
    Result := FPageAreaKind;
end;

{ TdxColumnHitTestManager }

procedure TdxColumnHitTestManager.CalcHitTestAmongNestedBoxes;
var
  ATable: TdxTableViewInfo;
  ARows: TdxRowCollection;
  ARowIndex: Integer;
  ARow: TdxRow;
  ATableCellRow: TdxTableCellRow;
begin
  ATable := GetActiveTableViewInfo;
  if ATable = nil then
  begin
    ARows := Column.GetOwnRows;
    try
      ARowIndex := Calculator.FastHitTestRowIndex(ARows);
      if ARowIndex >= 0 then
      begin
        ARow := ARows[ARowIndex];
        if ARow.Bounds.Contains(HitTestRequest.LogicalPoint) or not ARow.IsTabelCellRow then
          Calculator.CalcHitTest(ARow)
        else
        begin
          ATableCellRow := TdxTableCellRow(ARow);
          ATable := GetTable(ATableCellRow);
          if not CalcHitTestAmongNestedBoxesCore(ATable, False) then
            Calculator.CalcHitTest(ARow);
        end;
      end
      else
        ARows.RegisterFailedItemHitTest(Calculator);
    finally
      if Column.Rows <> ARows then
        ARows.Free;
    end;
  end
  else
    CalcHitTestAmongNestedBoxesCore(ATable, True);
end;

procedure TdxColumnHitTestManager.RegisterFailedHitTest;
begin
  HitTestResult.Column := nil;
end;

procedure TdxColumnHitTestManager.RegisterSuccessfullHitTest(
  AStrictMatch: Boolean);
begin
  HitTestResult.Column := Column;
  HitTestResult.IncreaseDetailsLevel(TdxDocumentLayoutDetailsLevel.Column);
  if AStrictMatch then
    HitTestResult.Accuracy := HitTestResult.Accuracy or ExactColumn;
end;

function TdxColumnHitTestManager.CalcHitTestAmongNestedBoxesCore(ATable: TdxTableViewInfo;
  AStrictTableMatch: Boolean): Boolean;
var
  AStrictTableRowMatch, AStrictTableCellMatch: Boolean;
begin
  AStrictTableRowMatch := (HitTestRequest.Accuracy and ExactTableRow) <> 0;
  AStrictTableCellMatch := (HitTestRequest.Accuracy and ExactTableCell) <> 0;
  Result := CalcHitTestAmongNestedBoxesCore(ATable, AStrictTableMatch, AStrictTableRowMatch, AStrictTableCellMatch);
end;

function TdxColumnHitTestManager.CalcHitTestAmongNestedBoxesCore(ATable: TdxTableViewInfo;
  AStrictTableMatch, AStrictTableRowMatch, AStrictTableCellMatch: Boolean): Boolean;
var
  ALogicalPoint: TPoint;
  ATableRowIndex, ATableCellIndex, I: Integer;
  ATableRow: TdxTableRowViewInfoBase;
  ATableCell: TdxTableCellViewInfo;
  AInnerTables: TdxTableViewInfoCollection;
  ACurrentTable: TdxTableViewInfo;
  ARows: TdxRowCollection;
  ATableRowAnchorComparable: TdxTableRowAnchorComparable;
  ATableCellAnchorComparable: TdxTableCellAnchorComparable;
begin
  if ATable.RowCount = 0 then
    Exit(False);

  ALogicalPoint := HitTestRequest.LogicalPoint;
  ATableRowAnchorComparable := TdxTableRowAnchorComparable.Create(ALogicalPoint.Y, ATable.Table.Rows.Last);
  try
    ATableRowIndex := Calculator.FastHitTestIndexCore<TdxTableRowViewInfoBase>(ATable.Rows, 0, ATable.RowCount, ATableRowAnchorComparable, AStrictTableRowMatch);
  finally
    ATableRowAnchorComparable.Free;
  end;

  if ATableRowIndex < 0 then
  begin
    HitTestResult.TableRow := nil;
    Exit(False);
  end;

  ATableRow := ATable.Rows[ATableRowIndex];

  ATableCellAnchorComparable := TdxTableCellAnchorComparable.Create(ALogicalPoint.X);
  try
    ATableCellIndex := Calculator.FastHitTestIndexCore<TdxTableCellViewInfo>(ATableRow.Cells, ATableCellAnchorComparable, AStrictTableCellMatch);
  finally
    ATableCellAnchorComparable.Free;
  end;
  if ATableCellIndex < 0 then
  begin
    HitTestResult.TableCell := nil;
    Exit(False);
  end;

  ATableCell := ATableRow.Cells[ATableCellIndex];
  if ATableCell.Left <= ALogicalPoint.X then
  begin
    AInnerTables := ATableCell.InnerTables;
    for I := 0 to AInnerTables.Count - 1 do
    begin
      ACurrentTable := AInnerTables[I];
      if CalcHitTestAmongNestedBoxesCore(ACurrentTable, AStrictTableMatch, True, True) then
        Exit(True);
    end;
  end;

  HitTestResult.IncreaseDetailsLevel(TdxDocumentLayoutDetailsLevel.TableRow);
  if AStrictTableMatch then
    HitTestResult.Accuracy := HitTestResult.Accuracy or ExactTableRow;
  HitTestResult.TableRow := ATableRow;

  HitTestResult.IncreaseDetailsLevel(TdxDocumentLayoutDetailsLevel.TableCell);
  if AStrictTableMatch then
    HitTestResult.Accuracy := HitTestResult.Accuracy or ExactTableCell;
  HitTestResult.TableCell := ATableCell;

  ARows := ATableCell.GetRows(Column);
  try
    ICalculator.ProcessRowCollection(ARows);
  finally
    ARows.Free;
  end;
  Result := True;
end;

function TdxColumnHitTestManager.GetActiveTableViewInfo: TdxTableViewInfo;
var
  ATables: TdxTableViewInfoCollection;
  ALogicalY, I, AActualTableBottom: Integer;
  ACurrentTable: TdxTableViewInfo;
begin
  ATables := Column.InnerTables;
  if (ATables = nil) or (ATables.Count = 0) then
    Exit(nil);
  ALogicalY := HitTestRequest.LogicalPoint.Y;
  for I := 0 to ATables.Count - 1 do
  begin
    ACurrentTable := ATables[I];
    AActualTableBottom := ACurrentTable.GetActualBottomPosition;
    if (ACurrentTable.Anchors.First.VerticalPosition <= ALogicalY) and (AActualTableBottom >= ALogicalY) then
      Exit(ACurrentTable);
  end;
  Result := nil;
end;

function TdxColumnHitTestManager.GetColumn: TdxColumn;
begin
  Result := TdxColumn(Box);
end;

function TdxColumnHitTestManager.GetTable(ATableCellRow: TdxTableCellRow): TdxTableViewInfo;
begin
  Result := ATableCellRow.CellViewInfo.TableViewInfo;
  while Result.ParentTableCellViewInfo <> nil do
    Result := Result.ParentTableCellViewInfo.TableViewInfo;
end;

{ TdxRowHitTestManager }

procedure TdxRowHitTestManager.CalcHitTestAmongNestedBoxes;
begin
  ICalculator.ProcessBoxCollection(Row.Boxes);
end;

procedure TdxRowHitTestManager.RegisterSuccessfullHitTest(AStrictMatch: Boolean);
begin
  HitTestResult.Row := Row;
  HitTestResult.IncreaseDetailsLevel(TdxDocumentLayoutDetailsLevel.Row);
  if AStrictMatch then
    HitTestResult.Accuracy := HitTestResult.Accuracy or ExactRow;
end;

procedure TdxRowHitTestManager.RegisterFailedHitTest;
begin
  HitTestResult.Row := nil;
end;

function TdxRowHitTestManager.GetRow: TdxRow;
begin
  Result := TdxRow(Box);
end;

{ TdxCharacterBoxHitTestManager }

procedure TdxCharacterBoxHitTestManager.CalcHitTestAmongNestedBoxes;
begin
//do nothing
end;

procedure TdxCharacterBoxHitTestManager.RegisterSuccessfullHitTest(AStrictMatch: Boolean);
begin
  HitTestResult.Character := Character;
  HitTestResult.IncreaseDetailsLevel(TdxDocumentLayoutDetailsLevel.Character);
  if AStrictMatch then
    HitTestResult.Accuracy := HitTestResult.Accuracy or ExactCharacter;
end;

procedure TdxCharacterBoxHitTestManager.RegisterFailedHitTest;
begin
  HitTestResult.Character := nil;
end;

function TdxCharacterBoxHitTestManager.GetCharacter: TdxCharacterBox;
begin
  Result := TdxCharacterBox(Box);
end;

{ TdxEmptyHitTestManager }

procedure TdxEmptyHitTestManager.CalcHitTestAmongNestedBoxes;
begin
//do nothing
end;

procedure TdxEmptyHitTestManager.RegisterSuccessfullHitTest(AStrictMatch: Boolean);
begin
//do nothing
end;

procedure TdxEmptyHitTestManager.RegisterFailedHitTest;
begin
//do nothing
end;

end.
