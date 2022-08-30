{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
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

unit dxSpreadSheetFormatXLSFormulas;

{$I cxVer.inc}

interface

{$UNDEF CXTEST}
{$DEFINE LOGNAMESONLY}
uses
  Types, Classes, SysUtils, Dialogs, Math, dxCore, cxClasses, Generics.Defaults, Generics.Collections, cxVariants, dxSpreadSheetFormatXLS,
  dxSpreadSheetFormatXLSTypes, dxSpreadSheetCore, dxSpreadSheetFormulas, dxSpreadSheetTypes, dxSpreadSheetFunctions,
  dxSpreadSheetClasses, dxSpreadSheetFormatXLSWriter, dxSpreadSheetFormatXLSReader, dxSpreadSheetCoreFormulas,
  dxSpreadSheetCoreFormulasTokens;

type
  TdxXLSTokenReaderProc = procedure of object;

  TdxSpreadSheetCustomFormulaAccess = class(TdxSpreadSheetCustomFormula);
  TdxSpreadSheetDefinedNameAccess = class(TdxSpreadSheetDefinedName);

  TdxXLSFormulaParameterDataType = (fpdtUnused, fpdtReference, fpdtValue, fpdtArray, fpdtAuto, fpdtSource);
  TdxXLSFormulaParameterDataTypes = set of TdxXLSFormulaParameterDataType;

  {TdxXLSFormulaReader}

  TdxXLSFormulaReader = class
  strict private const
    InvalidSourceOffset = -1;
  strict private
    FAnchorColumn: Integer;
    FAnchorRow: Integer;
    FArrays: TList<TPair<TdxSpreadSheetFormulaArrayToken, TSize>>;
    FCurrentToken: Byte;
    FFormula: TdxSpreadSheetCustomFormulaAccess;
    FIsBreak: Boolean;
    FIsError: Boolean;
    FOffset: Word;
    FOwner: TdxSpreadSheetXLSReader;
    FReader: TdxXLSReader;
    FSize: Integer;
    FSource: PByteArray;
    FSourceOffset: Integer;
    FTokenReaders: TList<TdxXLSTokenReaderProc>;
    FTokens: TList<TdxSpreadSheetFormulaToken>;

    function GetRecordReader: TdxXLSRecordReader; inline;
    function GetWords(AIndex: Integer): Word; inline;
  protected
    procedure AddFormula(AInfo: TdxSpreadSheetFunctionInfo; AParams: TdxSpreadSheetFormulaToken);
    procedure AddReader(AToken: Byte; AReader: TdxXLSTokenReaderProc);
    procedure AddToken(AToken: TdxSpreadSheetFormulaToken); overload;
    procedure AddToken(AToken: TdxSpreadSheetFormulaToken; AIncOffset: Integer); overload;
    function CheckUnknownNameFunction(AToken: TdxSpreadSheetFormulaToken): TdxSpreadSheetFormulaToken;
    procedure ClearTokens;
    procedure DecodeAreaReference(ASrcRow, ASrcColumn, ASrcRow2, ASrcColumn2: Word;
      var ARow, AColumn, ARow2, AColumn2: Integer; var AAbsRow, AAbsColumn, AAbsRow2, AAbsColumn2: Boolean); inline;
    procedure DecodeAreaReference2(ASrcRow, ASrcColumn, ASrcRow2, ASrcColumn2: Word;
      var ARow, AColumn, ARow2, AColumn2: Integer; var AAbsRow, AAbsColumn, AAbsRow2, AAbsColumn2: Boolean); inline;
    procedure DecodeReference(ASrcRow, ASrcColumn: Word;
      var ARow, AColumn: Integer; var AAbsRow, AAbsColumn: Boolean); inline;
    procedure DecodeReference2(ASrcRow, ASrcColumn: Word;
      var ARow, AColumn: Integer; var AAbsRow, AAbsColumn: Boolean); inline;
    procedure DoRead; virtual;
    function ExtractParameters(ACount: Integer): TdxSpreadSheetFormulaToken;
    function ExtractToken(AIndex: Integer = MaxInt): TdxSpreadSheetFormulaToken;
    procedure Read_Array;
    function Read_ArrayValue: TdxSpreadSheetFormulaToken;
    procedure Read_Area;
    procedure Read_AreaN;
    procedure Read_Area3d;
    procedure Read_Area3dErr;
    procedure Read_Attr;
    procedure Read_Bool;
    procedure Read_Exp;
    procedure Read_Err;
    procedure Read_Func;
    procedure Read_FuncVar;
    procedure Read_FuncVarV;
    procedure Read_Int;
    procedure Read_MemErr;
    procedure Read_MemFunc;
    procedure Read_MissArg;
    procedure Read_Name;
    procedure Read_NameX;
    procedure Read_Num;
    procedure Read_Operation;
    procedure Read_Paren;
    procedure Read_Ref;
    procedure Read_RefN;
    procedure Read_RefError;
    procedure Read_Ref3d;
    procedure Read_Ref3dErr;
    procedure Read_Str;
    procedure Read_Table;
    procedure Initialize;
  public
    constructor Create(AOwner: TdxSpreadSheetXLSReader); virtual;
    destructor Destroy; override;
    procedure ReadFormula(ACell: TdxSpreadSheetCell); overload;
    function ReadFormula(AFormula: TdxSpreadSheetCustomFormula; ASize: Integer): Boolean; overload;
    procedure ReadName(AName: TdxSpreadSheetDefinedName; ASize: Integer; ASource: PByteArray); virtual;

    property AnchorColumn: Integer read FAnchorColumn;
    property AnchorRow: Integer read FAnchorRow;
    property Arrays: TList<TPair<TdxSpreadSheetFormulaArrayToken, TSize>> read FArrays;
    property CurrentToken: Byte read FCurrentToken;
    property Formula: TdxSpreadSheetCustomFormulaAccess read FFormula;
    property IsBreak: Boolean read FIsBreak write FIsBreak;
    property IsError: Boolean read FIsError write FIsError;
    property Offset: Word read FOffset write FOffset;
    property Owner: TdxSpreadSheetXLSReader read FOwner;
    property Reader: TdxXLSReader read FReader;
    property RecordReader: TdxXLSRecordReader read GetRecordReader;
    property Size: Integer read FSize;
    property Source: PByteArray read FSource;
    property SourceOffset: Integer read FSourceOffset;
    property TokenReaders: TList<TdxXLSTokenReaderProc> read FTokenReaders;
    property Tokens: TList<TdxSpreadSheetFormulaToken> read FTokens;
    property Words[AIndex: Integer]: Word read GetWords;
  end;

  { TdxXLSFormulaWriter }

  TdxXLSFormulaTokenWriter = procedure(AToken: TdxSpreadSheetFormulaToken) of object;

  TdxXLSFormulaWriter = class
  strict private
    FAbsolute: Boolean;
    FAnchorColumn: Integer;
    FAnchorRow: Integer;
    FCurrentToken: TdxSpreadSheetFormulaToken;
    FFormula: TdxSpreadSheetCustomFormulaAccess;
    FOwner: TdxSpreadSheetXLSWriter;
    FSavePos: Int64;
    FStartPos: Int64;
    FWriter: TdxXLSWriter;
    FWriters: TDictionary<TClass, TdxXLSFormulaTokenWriter>;
    FWriteSize: Boolean;

    procedure EncodeReference(ARefRow, ARefColumn: TdxSpreadSheetReference; var ARow, AColumn: Word);
    procedure EncodeReferenceN(ARefRow, ARefColumn: TdxSpreadSheetReference; var ARow, AColumn: Integer);
    function GetAcceptedDataTypes: TdxXLSFormulaParameterDataTypes;
    function ValidateToken(AToken: Byte): Byte;
    procedure WriteNumericToken(const AValue: Double);
  protected
    procedure DoWriteTokens(ATokens: TdxSpreadSheetFormulaToken);
    function IsInvalidFormula: Boolean;
    procedure RegisterWriters;
    procedure WriteAttributeToken(AToken: TdxSpreadSheetFormulaToken);
    procedure WriteArea3DToken(AToken: TdxSpreadSheetFormulaToken);
    procedure WriteAreaToken(AToken: TdxSpreadSheetFormulaToken);
    procedure WriteBooleanToken(AToken: TdxSpreadSheetFormulaToken);
    procedure WriteCurrencyToken(AToken: TdxSpreadSheetFormulaToken);
    procedure WriteDateTimeToken(AToken: TdxSpreadSheetFormulaToken);
    procedure WriteErrorToken(AToken: TdxSpreadSheetFormulaToken);
    procedure WriteFloatToken(AToken: TdxSpreadSheetFormulaToken);
    procedure WriteIntegerToken(AToken: TdxSpreadSheetFormulaToken);
    procedure WriteFunctionToken(AToken: TdxSpreadSheetFormulaToken);
    procedure WriteNameToken(AToken: TdxSpreadSheetFormulaToken);
    procedure WriteNullToken(AToken: TdxSpreadSheetFormulaToken);
    procedure WriteOperationToken(AToken: TdxSpreadSheetFormulaToken);
    procedure WriteParenthesesToken(AToken: TdxSpreadSheetFormulaToken);
    procedure WriteRef3DToken(AToken: TdxSpreadSheetFormulaToken);
    procedure WriteRefToken(AToken: TdxSpreadSheetFormulaToken);
    procedure WriteRefNToken(AToken: TdxSpreadSheetFormulaToken);
    procedure WriteStringToken(AToken: TdxSpreadSheetFormulaToken);
    procedure WriteUnknownFunctionToken(AToken: TdxSpreadSheetFormulaToken);

    property AcceptedDataTypes: TdxXLSFormulaParameterDataTypes read GetAcceptedDataTypes;
    property CurrentToken: TdxSpreadSheetFormulaToken read FCurrentToken;
  public
    constructor Create(AOwner: TdxSpreadSheetXLSWriter; AWriteSize: Boolean = True); virtual;
    destructor Destroy; override;
    procedure WriteFormula(ACell: TdxSpreadSheetCell); overload;
    procedure WriteFormula(AFormula: TdxSpreadSheetCustomFormula; AAbsolute: Boolean); overload;
    procedure WriteFormula(AFormula: TdxSpreadSheetCustomFormula; AAbsolute: Boolean; AAnchorColumn, AAnchorRow: Integer); overload;
    procedure WriteNameFormula(AName: TdxSpreadSheetDefinedName); virtual;

    property Formula: TdxSpreadSheetCustomFormulaAccess read FFormula;
    property Writer: TdxXLSWriter read FWriter;
    property Writers: TDictionary<TClass, TdxXLSFormulaTokenWriter> read FWriters;
  end;


implementation

uses
  dxSpreadSheetCoreFormulasParser;

type
  TdxSpreadSheetFormulaReferenceAccess = class(TdxSpreadSheetFormulaReference);
  TdxSpreadSheetFormulaAreaReferenceAccess = class(TdxSpreadSheetFormulaAreaReference);

  TdxXLSFunctionParametersDefinition = record
    ID: Integer;
    Name: string;
    Parameters: array[0..2] of TdxXLSFormulaParameterDataTypes;
  end;

  PdxXLSFunctionParametersDefinition = ^TdxXLSFunctionParametersDefinition;

  TdxXLSFunctionsRepository = class
  private
    FDictionary: TDictionary<Integer, PdxXLSFunctionParametersDefinition>;
  protected
    procedure PrepareDefinition(AInfo: PdxXLSFunctionParametersDefinition);
  public
    constructor Create;
    destructor Destroy; override;

    function GetFormulaParameterType(AID, AIndex: Integer): TdxXLSFormulaParameterDataTypes;
  end;

{
  [ref] - [fpdtReference]
  (ref / val) - [fpdtReference, fpdtValue]
   val - [fpdtValue]
}

const
  Val    = [fpdtValue];
  Ref    = [fpdtReference];
  RefVal = [fpdtReference, fpdtValue];
  Same   = [];
  Unused = [fpdtUnused];

  XLSFunctionsDefinition: array[0..97] of TdxXLSFunctionParametersDefinition =
   ((ID: $0000; Name: 'COUNT';           Parameters: (RefVal, Same,   Same)),
    (ID: $0001; Name: 'IF';              Parameters: (Val,    RefVal, Same)),
//    (ID: $0004; Name: 'SUM';             Parameters: ([fpdtSource], Same,   Same)),
    (ID: $0004; Name: 'SUM';             Parameters: (RefVal, Same,   Same)),
    (ID: $0008; Name: 'ROW';             Parameters: (Ref,    Same,   Same)),
    (ID: $0009; Name: 'COLUMN';          Parameters: (Ref,    Same,   Same)),
    (ID: $000B; Name: 'NPV';             Parameters: (RefVal, Same,   Same)),
    (ID: $000C; Name: 'STDEV';           Parameters: (RefVal, Same,   Same)),
    (ID: $001C; Name: 'LOOKUP';          Parameters: (Val,    RefVal, RefVal)),
    (ID: $001D; Name: 'INDEX';           Parameters: (RefVal, Val,    Val)),
    (ID: $0024; Name: 'AND';             Parameters: (RefVal, Same,   Same)),
    (ID: $0025; Name: 'OR';              Parameters: (RefVal, Same,   Same)),
    (ID: $0028; Name: 'DCOUNT';          Parameters: (Ref,    RefVal, RefVal)),
    (ID: $0029; Name: 'DSUM';            Parameters: (RefVal, Same,   Same)),
    (ID: $002A; Name: 'DAVERAGE';        Parameters: (Ref,    RefVal, RefVal)),
    (ID: $002B; Name: 'DMIN';            Parameters: (Ref,    RefVal, RefVal)),
    (ID: $002C; Name: 'DMAX';            Parameters: (Ref,    RefVal, RefVal)),
    (ID: $002D; Name: 'DSTDEV';          Parameters: (Ref,    RefVal, RefVal)),
    (ID: $002E; Name: 'VAR';             Parameters: (RefVal, Same,   Same)),
    (ID: $002F; Name: 'DVAR';            Parameters: (Ref,    RefVal, RefVal)),
    (ID: $0031; Name: 'LINEST';          Parameters: (RefVal, Same,   Same)),
    (ID: $0032; Name: 'TREND';           Parameters: (RefVal, Same,   Same)),
    (ID: $0033; Name: 'LOGEST';          Parameters: (RefVal, Same,   Same)),
    (ID: $0034; Name: 'GROWTH';          Parameters: (RefVal, Same,   Same)),
    (ID: $0035; Name: 'GOTO';            Parameters: (Ref,    Unused, Unused)),
    (ID: $0037; Name: 'RETURN';          Parameters: (RefVal, Unused, Unused)),
    (ID: $0040; Name: 'MATCH';           Parameters: (Val,    RefVal, RefVal)),
    (ID: $004B; Name: 'AREAS';           Parameters: (Ref,    Unused, Unused)),
    (ID: $004C; Name: 'ROWS';            Parameters: (RefVal, Unused, Unused)),
    (ID: $004D; Name: 'COLUMNS';         Parameters: (RefVal, Unused, Unused)),
    (ID: $004E; Name: 'OFFSET';          Parameters: (Ref,    Val,    Val)),
    (ID: $004F; Name: 'ABSREF';          Parameters: (Val,    Ref,    Unused)),
    (ID: $0050; Name: 'RELREF';          Parameters: (Ref,    Ref,    Unused)),
    (ID: $0051; Name: 'ARGUMENT';        Parameters: (Val,    RefVal, Ref)),
    (ID: $0054; Name: 'ERROR';           Parameters: (Val,    RefVal, Unused)),
    (ID: $0058; Name: 'SET.NAME';        Parameters: (Val,    RefVal, Unused)),
    (ID: $005A; Name: 'DEREF';           Parameters: (Ref,    Unused, Unused)),
    (ID: $005C; Name: 'SERIES';          Parameters: (RefVal, Same,   Same)),
    (ID: $0064; Name: 'CHOOSE';          Parameters: (Val,    RefVal, RefVal)),
    (ID: $0065; Name: 'HLOOKUP';         Parameters: (Val,    RefVal, RefVal)),
    (ID: $0066; Name: 'VLOOKUP';         Parameters: (Val,    RefVal, RefVal)),
    (ID: $0069; Name: 'ISREF';           Parameters: (RefVal, Unused, Unused)),
    (ID: $006A; Name: 'GET.FORMULA';     Parameters: (RefVal, Unused, Unused)),
    (ID: $006C; Name: 'SET.VALUE';       Parameters: (Ref,    Val,    Unused)),
    (ID: $007D; Name: 'CELL';            Parameters: (Val,    Ref,    Unused)),
    (ID: $0082; Name: 'T';               Parameters: (RefVal, Unused, Unused)),
    (ID: $0083; Name: 'N';               Parameters: (RefVal, Unused, Unused)),
    (ID: $0092; Name: 'REFTEXT';         Parameters: (Ref,    Val,    Unused)),
    (ID: $0096; Name: 'CALL';            Parameters: (Val,    RefVal, RefVal)),
    (ID: $0098; Name: 'ADD.MENU';        Parameters: (Val,    RefVal, RefVal)),
    (ID: $0099; Name: 'ADD.COMMAND';     Parameters: (Val,    RefVal, RefVal)),
    (ID: $00A1; Name: 'DIALOG.BOX';      Parameters: (RefVal, Unused, Unused)),
    (ID: $00A9; Name: 'COUNTA';          Parameters: (RefVal, Same,   Same)),
    (ID: $00AA; Name: 'CANCEL.KEY';      Parameters: (Val,    Ref,    Unused)),
    (ID: $00AB; Name: 'FOR';             Parameters: (Val,    Same,   Same)),
    (ID: $00B1; Name: 'POKE';            Parameters: (Val,    RefVal, RefVal)),
    (ID: $00B7; Name: 'PRODUCT';         Parameters: (RefVal, Same,   Same)),
    (ID: $00B9; Name: 'GET.CELL';        Parameters: (Val,    Ref,    Unused)),
    (ID: $00BD; Name: 'DPRODUCT';        Parameters: (Ref,    RefVal, RefVal)),
    (ID: $00BF; Name: 'GET.NOTE';        Parameters: (RefVal, Same,   Same)),
    (ID: $00C0; Name: 'NOTE';            Parameters: (Val,    RefVal, RefVal)),
    (ID: $00C1; Name: 'STDEVP';          Parameters: (RefVal, Same,   Same)),
    (ID: $00C2; Name: 'VARP';            Parameters: (RefVal, Same,   Same)),
    (ID: $00C3; Name: 'DSTDEVP';         Parameters: (Ref,    RefVal, RefVal)),
    (ID: $00C4; Name: 'DVARP';           Parameters: (Ref,    RefVal, RefVal)),
    (ID: $00C7; Name: 'DCOUNTA';         Parameters: (Ref,    RefVal, RefVal)),
    (ID: $00D8; Name: 'RANK';            Parameters: (Val,    Ref,    Val)),
    (ID: $00E2; Name: 'FOR.CELL';        Parameters: (Val,    RefVal, RefVal)),
    (ID: $00E3; Name: 'MEDIAN';          Parameters: (RefVal, Same,   Same)),
    (ID: $00EB; Name: 'DGET';            Parameters: (Ref,    RefVal, RefVal)),
    (ID: $00EC; Name: 'CREATE.OBJECT';   Parameters: (Val,    RefVal, Unused)),
    (ID: $00F1; Name: 'FORMULA.CONVERT'; Parameters: (Val,    RefVal, RefVal)),
    (ID: $00FC; Name: 'FREQUENCY';       Parameters: (RefVal, Same,   Same)),
    (ID: $010D; Name: 'AVEDEV';          Parameters: (RefVal, Same,   Same)),
    (ID: $013E; Name: 'DEVSQ';           Parameters: (RefVal, Same,   Same)),
    (ID: $013F; Name: 'GEOMEAN';         Parameters: (RefVal, Same,   Same)),
    (ID: $0140; Name: 'HARMEAN';         Parameters: (RefVal, Same,   Same)),
    (ID: $0141; Name: 'SUMSQ';           Parameters: (RefVal, Same,   Same)),
    (ID: $0142; Name: 'KURT';            Parameters: (RefVal, Same,   Same)),
    (ID: $0143; Name: 'SKEW';            Parameters: (RefVal, Same,   Same)),
    (ID: $0144; Name: 'ZTEST';           Parameters: (RefVal, Val,    Val)),
    (ID: $0145; Name: 'LARGE';           Parameters: (RefVal, Val,    Unused)),
    (ID: $0146; Name: 'SMALL';           Parameters: (RefVal, Val,    Unused)),
    (ID: $0147; Name: 'QUARTILE';        Parameters: (RefVal, Val,    Unused)),
    (ID: $0148; Name: 'PERCENTILE';      Parameters: (RefVal, Val,    Unused)),
    (ID: $0149; Name: 'PERCENTRANK';     Parameters: (RefVal, Val,    Val)),
    (ID: $014B; Name: 'TRIMMEAN';        Parameters: (RefVal, Val,    Unused)),
    (ID: $0158; Name: 'SUBTOTAL';        Parameters: (Val,    Ref,    Ref)),
    (ID: $0159; Name: 'SUMIF';           Parameters: (Ref,    Val,    Ref)),
    (ID: $015A; Name: 'COUNTIF';         Parameters: (Ref,    Val,    Unused)),
    (ID: $015B; Name: 'COUNTBLANK';      Parameters: (Ref,    Unused, Unused)),
    (ID: $0168; Name: 'PHONETIC';        Parameters: (Ref,    Unused, Unused)),
    (ID: $0169; Name: 'AVERAGEA';        Parameters: (RefVal, Same,   Same)),
    (ID: $016A; Name: 'MAXA';            Parameters: (RefVal, Same,   Same)),
    (ID: $016B; Name: 'MINA';            Parameters: (RefVal, Same,   Same)),
    (ID: $016C; Name: 'STDEVPA';         Parameters: (RefVal, Same,   Same)),
    (ID: $016D; Name: 'VARPA';           Parameters: (RefVal, Same,   Same)),
    (ID: $016E; Name: 'STDEVA';          Parameters: (RefVal, Same,   Same)),
    (ID: $016F; Name: 'VARA';            Parameters: (RefVal, Same,   Same)));




var
  Repository: TdxXLSFunctionsRepository;


{TdxXLSFormulaReader}

constructor TdxXLSFormulaReader.Create(AOwner: TdxSpreadSheetXLSReader);
begin
  FOwner := AOwner;
  FReader := Owner.Reader;
  FArrays := TList<TPair<TdxSpreadSheetFormulaArrayToken, TSize>>.Create;
  FTokens := TList<TdxSpreadSheetFormulaToken>.Create;
  FTokenReaders := TList<TdxXLSTokenReaderProc>.Create;
  TokenReaders.Count := $FF;
  Initialize;
end;

destructor TdxXLSFormulaReader.Destroy;
begin
  ClearTokens;
  FreeAndNil(FArrays);
  FreeAndNil(FTokenReaders);
  FreeAndNil(FTokens);
  inherited Destroy;
end;

procedure TdxXLSFormulaReader.ReadFormula(ACell: TdxSpreadSheetCell);
var
  AFormula: TdxSpreadSheetFormula;
begin
  AFormula := TdxSpreadSheetFormula.Create(ACell);
  if ReadFormula(AFormula, Reader.ReadWord) then
    ACell.AsFormula := AFormula
  else
    AFormula.Free;
end;

function TdxXLSFormulaReader.ReadFormula(AFormula: TdxSpreadSheetCustomFormula; ASize: Integer): Boolean;
begin
  FSize := ASize;
  FFormula := TdxSpreadSheetCustomFormulaAccess(AFormula);
  FAnchorRow := AFormula.AnchorRow;
  FAnchorColumn := AFormula.AnchorColumn;
  FSourceOffset := RecordReader.Position;
  FSource := @PByteArray(RecordReader.Memory)^[RecordReader.Position];
  DoRead;
  Result := FFormula <> nil;
end;

procedure TdxXLSFormulaReader.ReadName(AName: TdxSpreadSheetDefinedName; ASize: Integer; ASource: PByteArray);
begin
  FFormula := TdxSpreadSheetCustomFormulaAccess(TdxSpreadSheetDefinedNameFormula.Create(AName));
  try
    FSize := ASize;
    FAnchorRow := 0;
    FAnchorColumn := 0;
    FSource := ASource;
    FSourceOffset := InvalidSourceOffset;
    DoRead;
  finally
    TdxSpreadSheetDefinedNameAccess(AName).Formula := TdxSpreadSheetDefinedNameFormula(Formula);
  end;
end;

procedure TdxXLSFormulaReader.AddFormula(AInfo: TdxSpreadSheetFunctionInfo; AParams: TdxSpreadSheetFormulaToken);
var
  AToken: TdxSpreadSheetFormulaToken;
begin
  if AInfo <> nil then
    AToken := TdxSpreadSheetFormulaFunctionToken.Create(AInfo)
  else
    AToken := TdxSpreadSheetFormulaUnknownFunctionToken.Create('');

  TdxSpreadSheetFormulaToken.AddChild(AToken, AParams);
  AddToken(AToken);
end;

procedure TdxXLSFormulaReader.AddReader(AToken: Byte; AReader: TdxXLSTokenReaderProc);
begin
  TokenReaders[AToken] := AReader;
end;

procedure TdxXLSFormulaReader.AddToken(AToken: TdxSpreadSheetFormulaToken);
begin
  Tokens.Add(AToken);
end;

procedure TdxXLSFormulaReader.AddToken(AToken: TdxSpreadSheetFormulaToken; AIncOffset: Integer);
begin
  AddToken(AToken);
  Offset := Offset + AIncOffset;
end;

function TdxXLSFormulaReader.CheckUnknownNameFunction(AToken: TdxSpreadSheetFormulaToken): TdxSpreadSheetFormulaToken;
begin
  Result := AToken;
  if AToken is TdxSpreadSheetDefinedNameToken then
  try
    Tokens.Delete(Tokens.Count - 1);
    AddToken(TdxSpreadSheetFormulaUnknownFunctionToken.Create(TdxSpreadSheetDefinedNameToken(AToken).Value));
    Result := Tokens.Last;
  finally
    AToken.Free;
  end;
end;

procedure TdxXLSFormulaReader.ClearTokens;
var
  I: Integer;
begin
  for I := 0 to Tokens.Count - 1 do
    Tokens[I].Free;
  Tokens.Clear;
end;

procedure TdxXLSFormulaReader.DecodeAreaReference(ASrcRow, ASrcColumn, ASrcRow2, ASrcColumn2: Word;
  var ARow, AColumn, ARow2, AColumn2: Integer; var AAbsRow, AAbsColumn, AAbsRow2, AAbsColumn2: Boolean);
begin
  DecodeReference(ASrcRow, ASrcColumn, ARow, AColumn, AAbsRow, AAbsColumn);
  DecodeReference(ASrcRow2, ASrcColumn2, ARow2, AColumn2, AAbsRow2, AAbsColumn2);
  if (AColumn = 0) and (AColumn2 >= 255) then
    AColumn2 := MaxInt;
end;

procedure TdxXLSFormulaReader.DecodeAreaReference2(ASrcRow, ASrcColumn, ASrcRow2, ASrcColumn2: Word;
  var ARow, AColumn, ARow2, AColumn2: Integer; var AAbsRow, AAbsColumn, AAbsRow2, AAbsColumn2: Boolean);
begin
  DecodeReference2(ASrcRow, ASrcColumn, ARow, AColumn, AAbsRow, AAbsColumn);
  DecodeReference2(ASrcRow2, ASrcColumn2, ARow2, AColumn2, AAbsRow2, AAbsColumn2);
end;

procedure TdxXLSFormulaReader.DecodeReference(ASrcRow, ASrcColumn: Word; var ARow, AColumn: Integer; var AAbsRow, AAbsColumn: Boolean);
begin
  ARow := SmallInt(ASrcRow);
  AColumn := ASrcColumn and $3FFF;
  AAbsRow := (ASrcColumn and $8000) = 0;
  AAbsColumn := (ASrcColumn and $4000) = 0;
  // all cells in row
  if AColumn > 255 then
    AColumn := MaxInt;
  // all cells in column
  if ARow < 0 then
    ARow := MaxInt;
  if not AAbsRow then
    ARow := ARow - AnchorRow;
  if not AAbsColumn then
    AColumn := AColumn - AnchorColumn;
end;

procedure TdxXLSFormulaReader.DecodeReference2(ASrcRow, ASrcColumn: Word; var ARow, AColumn: Integer; var AAbsRow, AAbsColumn: Boolean);
begin
  ARow := SmallInt(ASrcRow);
  AColumn := Shortint(Byte(ASrcColumn and $3FFF));
  AAbsRow := (ASrcColumn and $8000) = 0;
  AAbsColumn := (ASrcColumn and $4000) = 0;
  if not AAbsColumn and ((AColumn + AnchorColumn) >= 256) then
    AColumn := AColumn - 256
end;

procedure TdxXLSFormulaReader.DoRead;
var
  ATokenReader: TdxXLSTokenReaderProc;
begin
  if (Source^[0] = ptgExp) and (Size <= 5) then
  begin
    FFormula := nil;
    Exit;
  end;
  Offset := 0;
  while (Offset < Size) and not IsBreak and not IsError do
  begin
    FCurrentToken := Source^[FOffset];
    Inc(FOffset);
    ATokenReader := TokenReaders[CurrentToken];
    IsError := not Assigned(ATokenReader);
    if IsError then Break;
    ATokenReader;
  end;
  while not IsBreak and not IsError and (Arrays.Count > 0) do
  begin
    Inc(FOffset);
    Read_Table;
  end;
  if not IsError and (Tokens.Count = 1) then
    Formula.FTokens := ExtractToken(0);
end;

function TdxXLSFormulaReader.ExtractParameters(ACount: Integer): TdxSpreadSheetFormulaToken;
var
  I: Integer;
  AParameter: TdxSpreadSheetFormulaToken;
begin
  Result := nil;
  ACount := Min(ACount, Tokens.Count);
  if ACount = 0 then
    Exit;

  for I := 0 to ACount - 1 do
  begin
    AParameter := TdxSpreadSheetFormulaToken.Create;
    TdxSpreadSheetFormulaToken.AddChild(AParameter, ExtractToken);
    if Result <> nil then
      TdxSpreadSheetFormulaToken.Add(AParameter, Result);
    Result := AParameter;
  end;
end;

function TdxXLSFormulaReader.ExtractToken(AIndex: Integer = MaxInt): TdxSpreadSheetFormulaToken;
begin
  if AIndex > Tokens.Count then
    AIndex := Tokens.Count - 1;
  Result := Tokens[AIndex];
  Tokens.Delete(AIndex);
  Result.Owner := Formula;
end;

procedure TdxXLSFormulaReader.Read_Array;
var
  ASize: TSize;
  AArray: TdxSpreadSheetFormulaArrayToken;
begin
  ASize.cx := Source^[Offset];
  if ASize.cx = 0 then
    ASize.cx := 256;
  ASize.cy:= PWord(@Source^[Offset + 1])^;
  AArray := TdxSpreadSheetFormulaArrayToken.Create;
  Arrays.Add(TPair<TdxSpreadSheetFormulaArrayToken, TSize>.Create(AArray, ASize));
  AddToken(AArray, 7);
end;

function TdxXLSFormulaReader.Read_ArrayValue: TdxSpreadSheetFormulaToken;
var
  AKey, ALen: Integer;
  AFloatValue: Double;
  AStringValue: string;
begin
  AKey := Source^[Offset];
  Inc(FOffset);
  if AKey = 1 then
  begin
    AFloatValue := PDouble(@Source^[Offset])^;
    Result := TdxSpreadSheetFormulaFloatValueToken.Create(AFloatValue);
    Inc(FOffset, 8);
  end
  else
    if AKey = 2 then
    begin
      ALen := Source^[Offset];
      SetLength(AStringValue, ALen);
      if ALen > 0 then
        Move(Source^[Offset + 1], AStringValue[1], ALen);
      Result := TdxSpreadSheetFormulaStringValueToken.Create(AStringValue);
      Inc(FOffset, ALen + 1);
    end
    else
      Result := TdxSpreadSheetFormulaNullToken.Create;
end;

procedure TdxXLSFormulaReader.Read_Area;
var
  ATokens: PWordArray;
  ARow, AColumn, ARow2, AColumn2: Integer;
  AbsRow, AbsColumn, AbsRow2, AbsColumn2: Boolean;
begin
  ATokens := @Source^[Offset];
  DecodeAreaReference(ATokens^[0], ATokens^[2], ATokens^[1], ATokens^[3],
    ARow, AColumn, ARow2, AColumn2, AbsRow, AbsColumn, AbsRow2, AbsColumn2);
  AddToken(TdxSpreadSheetFormulaAreaReference.Create(ARow, AColumn, ARow2, AColumn2,
    AbsRow, AbsColumn, AbsRow2, AbsColumn2));
  Inc(FOffset, 8);
end;

procedure TdxXLSFormulaReader.Read_AreaN;
var
  ATokens: PWordArray;
  ARow, AColumn, ARow2, AColumn2: Integer;
  AbsRow, AbsColumn, AbsRow2, AbsColumn2: Boolean;
begin
  ATokens := @Source^[Offset];
  DecodeAreaReference2(ATokens^[0], ATokens^[2], ATokens^[1], ATokens^[3],
    ARow, AColumn, ARow2, AColumn2, AbsRow, AbsColumn, AbsRow2, AbsColumn2);
  AddToken(TdxSpreadSheetFormulaAreaReference.Create(ARow, AColumn, ARow2, AColumn2,
    AbsRow, AbsColumn, AbsRow2, AbsColumn2));
  Inc(FOffset, 8);
end;

procedure TdxXLSFormulaReader.Read_Area3d;
var
  ATokens: PWordArray;
  ALink: TdxSpreadSheet3DReferenceLink;
  ARow, AColumn, ARow2, AColumn2: Integer;
  AbsRow, AbsColumn, AbsRow2, AbsColumn2: Boolean;
begin
  ATokens := @Source^[Offset];
  ALink := TdxSpreadSheet3DReferenceLink.Create(Owner.GetSheet(ATokens^[0]));
  DecodeAreaReference(ATokens^[1], ATokens^[3], ATokens^[2], ATokens^[4],
    ARow, AColumn, ARow2, AColumn2, AbsRow, AbsColumn, AbsRow2, AbsColumn2);
  AddToken(TdxSpreadSheetFormula3DAreaReference.Create(ALink, nil,
    ARow, AColumn, ARow2, AColumn2, AbsRow, AbsColumn, AbsRow2, AbsColumn2));
  Inc(FOffset, 10);
end;

procedure TdxXLSFormulaReader.Read_Area3dErr;
begin
  Read_Area3d;
  TdxSpreadSheetFormula3DAreaReference(Tokens.Last).IsError := True;
end;

procedure TdxXLSFormulaReader.Read_Attr;
//var
//  ASpaceAttribute, ASpaceCount: Byte;
begin
  if (Source^[FOffset] and $04) <> 0 then
    FOffset := FOffset + (PWord(@Source^[FOffset + 1])^ + 2) * 2 - 3
  else
    if (Source^[FOffset] and $40) <> 0 then
    begin
//      ASpaceAttribute := Source^[1];
//      Add(TdxSpreadSheetFormulaAttributeToken.Create(Source^[FOffset + 2])); todo:
    end
    else
      if (Source^[FOffset] and $10) <> 0 then
        AddFormula(dxSpreadSheetFunctionsRepository.GetInfoByID(4), ExtractParameters(1));
  Inc(FOffset, 3);
end;

procedure TdxXLSFormulaReader.Read_Bool;
begin
  AddToken(TdxSpreadSheetFormulaBooleanValueToken.Create(Boolean(Source^[FOffset])));
  Inc(FOffset);
end;

procedure TdxXLSFormulaReader.Read_Exp;
begin
  Inc(FOffset, 4);
end;

procedure TdxXLSFormulaReader.Read_Err;
begin
  AddToken(TdxSpreadSheetFormulaErrorValueToken.Create(XLSErrorToErrorCode(Source^[FOffset])));
  Inc(FOffset);
end;

procedure TdxXLSFormulaReader.Read_Func;
var
  AInfo: TdxSpreadSheetFunctionInfo;
  AParams: TdxSpreadSheetFormulaToken;
  AParamCount: Integer;
  AParamKind: TdxSpreadSheetFunctionParamKindInfo;
begin
  AInfo := dxSpreadSheetFunctionsRepository.GetInfoByID(PWord(@Source[Offset])^);
  Inc(FOffset, 2);
  AParams := nil;
  if AInfo <> nil then
  begin
    AInfo.ParamInfo(AParamCount, AParamKind);
    AParams := ExtractParameters(AParamCount);
  end;
  AddFormula(AInfo, AParams);
end;

procedure TdxXLSFormulaReader.Read_FuncVar;
var
  ACount: Byte;
begin
  ACount := Source[Offset] and $7F;
  Inc(FOffset);
  AddFormula(dxSpreadSheetFunctionsRepository.GetInfoByID(PWord(@Source^[FOffset])^ and $7FFF),
    ExtractParameters(ACount));
  Inc(FOffset, 2);
end;

procedure TdxXLSFormulaReader.Read_FuncVarV;
var
  ID: Word;
  ACount: Byte;
  AParams: TdxSpreadSheetFormulaToken;
begin
  ACount := Source[Offset] and $7F;
  Inc(FOffset);
  ID := PWord(@Source^[FOffset])^ and $7FFF;
  if ID = 255 then
  begin
    AParams := ExtractParameters(ACount - 1);
    TdxSpreadSheetFormulaToken.AddChild(CheckUnknownNameFunction(Tokens.Last), AParams);
  end
  else
    AddFormula(dxSpreadSheetFunctionsRepository.GetInfoByID(ID), ExtractParameters(ACount));

  Inc(FOffset, 2);
end;

procedure TdxXLSFormulaReader.Read_Int;
begin
  AddToken(TdxSpreadSheetFormulaIntegerValueToken.Create(PWord(@FSource[FOffset])^), 2);
end;

procedure TdxXLSFormulaReader.Read_MemErr;
begin
  Inc(FOffset, 6);
end;

procedure TdxXLSFormulaReader.Read_MemFunc;
begin
  Inc(FOffset, 2);
end;

procedure TdxXLSFormulaReader.Read_MissArg;
begin
  AddToken(TdxSpreadSheetFormulaNullToken.Create());
end;

procedure TdxXLSFormulaReader.Read_Name;
var
  AName: string;
  AInfo: TdxSpreadSheetFunctionInfo;
begin
  AName := Owner.GetName(PWord(@Source^[FOffset])^ - 1, AInfo);
  if AInfo = nil then
    AddToken(TdxSpreadSheetDefinedNameToken.Create(AName, Owner.SpreadSheet.DefinedNames))
  else
    AddToken(TdxSpreadSheetUnknownNameToken.Create(AName));
  Inc(FOffset, 4);
end;

procedure TdxXLSFormulaReader.Read_NameX;
var
  AInfo: TdxSpreadSheetFunctionInfo;
  AName: string;
begin
  Inc(FOffset, 2); // skip, SheetIndex
  AName := Owner.GetExternalName(PWord(@Source^[FOffset])^ - 1, AInfo);
  if AInfo = nil then
    AddToken(TdxSpreadSheetDefinedNameToken.Create(AName, Owner.SpreadSheet.DefinedNames))
  else
    AddToken(TdxSpreadSheetUnknownNameToken.Create(AName));
  Inc(FOffset, 4);
end;

procedure TdxXLSFormulaReader.Read_Num;
begin
  AddToken(TdxSpreadSheetFormulaFloatValueToken.Create(PDouble(@Source^[Offset])^), 8);
end;

procedure TdxXLSFormulaReader.Read_Operation;
var
  ALastToken, AOperation: TdxSpreadSheetFormulaToken;
begin
  AOperation := TdxSpreadSheetFormulaOperationToken.Create(TdxSpreadSheetFormulaOperation(CurrentToken - ptgAdd));
  case CurrentToken of
    ptgAdd..ptgRange:
      begin
        ALastToken := ExtractToken();
        TdxSpreadSheetFormulaToken.Append(Tokens.Last, ALastToken);
        TdxSpreadSheetFormulaToken.Append(ALastToken, AOperation);
      end;
    ptgUPlus..ptgPercent:
      TdxSpreadSheetFormulaToken.Add(Tokens.Last, AOperation);
  end;
end;

procedure TdxXLSFormulaReader.Read_Paren;
var
  AToken: TdxSpreadSheetFormulaToken;
begin
  AToken := TdxSpreadSheetFormulaParenthesesToken.Create;
  TdxSpreadSheetFormulaToken.AddChild(AToken, ExtractToken);
  AddToken(AToken);
end;

procedure TdxXLSFormulaReader.Read_Ref;
var
  ARow, AColumn: Integer;
  AAbsRow, AAbsColumn: Boolean;
begin
  DecodeReference(PWordArray(@FSource^[Offset])^[0], PWordArray(@FSource^[Offset])^[1],
    ARow, AColumn, AAbsRow, AAbsColumn);
  AddToken(TdxSpreadSheetFormulaReference.Create(ARow, AColumn, AAbsRow, AAbsColumn));
  Inc(FOffset, 4);
end;

procedure TdxXLSFormulaReader.Read_RefN;
var
  ARow, AColumn: Integer;
  AAbsRow, AAbsColumn: Boolean;
begin
  DecodeReference2(PWordArray(@FSource^[Offset])^[0], PWordArray(@FSource^[Offset])^[1],
    ARow, AColumn, AAbsRow, AAbsColumn);
  AddToken(TdxSpreadSheetFormulaReference.Create(ARow, AColumn, AAbsRow, AAbsColumn));
  Inc(FOffset, 4);
end;

procedure TdxXLSFormulaReader.Read_RefError;
begin
  AddToken(TdxSpreadSheetFormulaErrorValueToken.Create(ecRefErr));
  Inc(FOffset, 4);
end;

procedure TdxXLSFormulaReader.Read_Ref3d;
var
  ATokens: PWordArray;
  ARow, AColumn: Integer;
  AAbsRow, AAbsColumn: Boolean;
  ALink: TdxSpreadSheet3DReferenceLink;
begin
  ATokens := @Source^[FOffset];
  DecodeReference(ATokens^[1], ATokens^[2], ARow, AColumn, AAbsRow, AAbsColumn);
  ALink := TdxSpreadSheet3DReferenceLink.Create(Owner.GetSheet(ATokens^[0]));
  AddToken(TdxSpreadSheetFormula3DReference.Create(ALink, ARow, AColumn, AAbsRow, AAbsColumn));
  Inc(FOffset, 6);
end;

procedure TdxXLSFormulaReader.Read_Ref3dErr;
begin
  Read_Ref3d;
  TdxSpreadSheetFormula3DReference(Tokens.Last).IsError := True;
end;

procedure TdxXLSFormulaReader.Read_Str;
var
  ALength: Byte;
  AOptions: Byte;
  AValue: string;
  AValueAnsi: AnsiString;
begin
  if SourceOffset = InvalidSourceOffset then
  begin
    ALength := FSource^[FOffset];
    AOptions := FSource^[FOffset + 1];
    Inc(FOffset, 2);

    if AOptions and $01 <> 0 then
    begin
      SetLength(AValue, ALength);
      Move(FSource^[FOffset], PWideChar(AValue)^, ALength * SizeOf(WideChar));
      Inc(FOffset, ALength * SizeOf(WideChar));
    end
    else
    begin
      SetLength(AValueAnsi, ALength);
      Move(FSource^[FOffset], PAnsiChar(AValueAnsi)^, ALength);
      AValue := dxAnsiStringToString(AValueAnsi, Owner.CodePage);
      Inc(FOffset, ALength);
    end;
  end
  else
  begin
    AValue := Reader.XLS_ReadSimpleString(FOffset + SourceOffset);
    FOffset := RecordReader.Position - SourceOffset;
  end;
  AddToken(TdxSpreadSheetFormulaStringValueToken.Create(AValue));
end;

procedure TdxXLSFormulaReader.Read_Table;

  procedure ReadArray(AArray: TdxSpreadSheetFormulaArrayToken; const ASize: TSize);
  var
    ARow, AColumn: Integer;
    AParameter: TdxSpreadSheetFormulaToken;
  begin
    for ARow := 0 to ASize.cy do
    begin
      for AColumn := 0 to ASize.cx do
      begin
        AParameter := TdxSpreadSheetFormulaToken.Create;
        TdxSpreadSheetFormulaToken.AddChild(AParameter, Read_ArrayValue);
        TdxSpreadSheetFormulaToken.AddChild(AArray, AParameter);
      end;
      if ARow < ASize.cy then
        TdxSpreadSheetFormulaToken.AddChild(AArray, TdxSpreadSheetFormulaArrayRowSeparator.Create);
     end;
     Arrays.Delete(0);
  end;

begin
  Inc(FOffset, 2);
  if Arrays.Count > 0 then
    ReadArray(Arrays[0].Key, Arrays[0].Value)
  else
    IsBreak := True;
end;

procedure TdxXLSFormulaReader.Initialize;
var
  I: Integer;
begin
  for I := ptgAdd to ptgPercent do
    AddReader(I, Read_Operation);

  AddReader(ptgArray, Read_Array);
  AddReader(ptgArrayA, Read_Array);
  AddReader(ptgAttr, Read_Attr);
  AddReader(ptgBool, Read_Bool);
  AddReader(ptgExp, Read_Exp);
  //
  AddReader(ptgFunc, Read_Func);
  AddReader(ptgFuncV, Read_Func);
  AddReader(ptgFuncA, Read_Func);
  AddReader(ptgFuncVar, Read_FuncVar);
  AddReader(ptgFuncVarV, Read_FuncVar);
  AddReader(ptgFuncVarV, Read_FuncVarV);
  AddReader(ptgFuncVarA, Read_FuncVar);
  //
  AddReader(ptgInt, Read_Int);
  //
  AddReader(ptgMemFunc, Read_MemFunc);
  AddReader(ptgMemNoMemN, Read_MemFunc);
  AddReader(ptgMemErr, Read_MemErr);
  AddReader(ptgMemArea, Read_MemErr);
  AddReader(ptgMemAreaN, Read_MemErr);
  AddReader(ptgMemAreaA, Read_MemErr);
  //
  AddReader(ptgName, Read_Name);
  AddReader(ptgNameV, Read_Name);
  AddReader(ptgNameA, Read_Name);
  AddReader(ptgNameX, Read_NameX);
  //
  AddReader(ptgNum, Read_Num);
  AddReader(ptgParen, Read_Paren);
  //
  AddReader(ptgArea, Read_Area);
  AddReader(ptgAreaA, Read_Area);
  AddReader(ptgAreaV, Read_Area);

  AddReader(ptgAreaN, Read_AreaN);
  AddReader(ptgAreaNA, Read_AreaN);
  AddReader(ptgAreaNV, Read_AreaN);

  AddReader(ptgArea3d, Read_Area3d);
  AddReader(ptgAreaErr3d, Read_Area3dErr);
   //
  AddReader(ptgRef, Read_Ref);
  AddReader(ptgRefA, Read_Ref);
  AddReader(ptgRefV, Read_Ref);

  AddReader(ptgRefN, Read_RefN);
  AddReader(ptgRefNA, Read_RefN);
  AddReader(ptgRefNV, Read_RefN);
  //
  AddReader(ptgRefErr, Read_RefError);
  AddReader(ptgRefErrA, Read_RefError);
  AddReader(ptgRefErrV, Read_RefError);
  //
  AddReader(ptgRef3d, Read_Ref3d);
  AddReader(ptgRef3dA, Read_Ref3d);
  AddReader(ptgRef3dV, Read_Ref3d);
  //
  AddReader(ptgRefErr3d, Read_Ref3dErr);
  AddReader(ptgRefErr3dA, Read_Ref3dErr);
  AddReader(ptgRefErr3dV, Read_Ref3dErr);
  //
  AddReader(ptgTbl, Read_Table);
  AddReader(ptgStr, Read_Str);
  AddReader(ptgErr, Read_Err);
  AddReader(ptgMissArg, Read_MissArg);
end;

function TdxXLSFormulaReader.GetRecordReader: TdxXLSRecordReader;
begin
  Result := Owner.RecordReader;
end;

function TdxXLSFormulaReader.GetWords(AIndex: Integer): Word;
begin
  Result := PWordArray(FSource[Offset])^[AIndex];
end;

{ TdxXLSFormulaWriter }

constructor TdxXLSFormulaWriter.Create(AOwner: TdxSpreadSheetXLSWriter; AWriteSize: Boolean = True);
begin
  inherited Create;
  FOwner := AOwner;
  FWriter := AOwner.Writer;
  FWriteSize := AWriteSize;
  FWriters := TDictionary<TClass, TdxXLSFormulaTokenWriter>.Create;
  FSavePos := FWriter.Stream.Position;
  if FWriteSize then
    FWriter.WriteWord(0); // reserve for size
  RegisterWriters;
end;

destructor TdxXLSFormulaWriter.Destroy;
var
  APosition: Int64;
begin
  if FWriteSize then
  begin
    APosition := FWriter.Stream.Position;
    if Formula = nil then
      FStartPos := APosition;
    FWriter.Stream.Position := FSavePos;
    FWriter.WriteWord(APosition - FStartPos);
    FWriter.Stream.Position := APosition;
  end;
  FreeAndNil(FWriters);
  inherited Destroy;
end;

procedure TdxXLSFormulaWriter.WriteFormula(ACell: TdxSpreadSheetCell);
begin
  WriteFormula(ACell.AsFormula, False);
end;

procedure TdxXLSFormulaWriter.WriteFormula(AFormula: TdxSpreadSheetCustomFormula; AAbsolute: Boolean);
begin
  WriteFormula(AFormula, AAbsolute, AFormula.AnchorColumn, AFormula.AnchorRow);
end;

procedure TdxXLSFormulaWriter.WriteFormula(
  AFormula: TdxSpreadSheetCustomFormula; AAbsolute: Boolean; AAnchorColumn, AAnchorRow: Integer);
begin
  FFormula := TdxSpreadSheetCustomFormulaAccess(AFormula);
  FAbsolute := AAbsolute;
  FAnchorRow := AAnchorRow;
  FAnchorColumn := AAnchorColumn;
  FStartPos := Writer.Position;
  if IsInvalidFormula then
  begin
    Writer.WriteByte(ptgStr);
    Writer.XLS_WriteSimpleString(dxSpreadSheetFormulaExcludeEqualSymbol(AFormula.AsText), 1);
  end
  else
    DoWriteTokens(AFormula.Tokens);
end;

procedure TdxXLSFormulaWriter.WriteNameFormula(AName: TdxSpreadSheetDefinedName);
begin
  if TdxSpreadSheetDefinedNameAccess(AName).Formula <> nil then
    WriteFormula(TdxSpreadSheetDefinedNameAccess(AName).Formula, True);
end;

procedure TdxXLSFormulaWriter.DoWriteTokens(ATokens: TdxSpreadSheetFormulaToken);
var
  AToken: TdxSpreadSheetFormulaToken;
  AWriter: TdxXLSFormulaTokenWriter;
  AWriterExist: Boolean;
begin
  AToken := ATokens;
  while AToken <> nil do
  begin
    AWriterExist := Writers.TryGetValue(AToken.ClassType, AWriter);
    if AToken is TdxSpreadSheetFormulaUnknownFunctionToken then
      WriteUnknownFunctionToken(AToken)
    else
    begin
      DoWriteTokens(AToken.FirstChild);
      if AWriterExist then
      begin
        FCurrentToken := AToken;
        AWriter(AToken);
        FCurrentToken := nil;
      end;
    end;
    AToken := AToken.Next;
  end;
end;

function TdxXLSFormulaWriter.IsInvalidFormula: Boolean;

   function CheckTokens(AToken: TdxSpreadSheetFormulaToken): Boolean;
   begin
     Result := (AToken is TdxSpreadSheetFormulaUnknownFunctionToken) or
       (AToken is TdxSpreadSheetUnknownNameToken);
     if Result or (AToken = nil) then
       Exit;
     Result := CheckTokens(AToken.Next) or CheckTokens(AToken.FirstChild);
   end;

begin
  Result := CheckTokens(Formula.Tokens);
end;

procedure TdxXLSFormulaWriter.RegisterWriters;
begin
  Writers.Add(TdxSpreadSheetFormula3DAreaReference, WriteArea3DToken);
  Writers.Add(TdxSpreadSheetFormulaAreaReference, WriteAreaToken);
  Writers.Add(TdxSpreadSheetFormulaBooleanValueToken, WriteBooleanToken);
  Writers.Add(TdxSpreadSheetFormulaCurrencyValueToken, WriteCurrencyToken);
  Writers.Add(TdxSpreadSheetFormulaDateTimeValueToken, WriteDateTimeToken);
  Writers.Add(TdxSpreadSheetFormulaErrorValueToken, WriteErrorToken);
  Writers.Add(TdxSpreadSheetFormulaFloatValueToken, WriteFloatToken);
  Writers.Add(TdxSpreadSheetFormulaFunctionToken, WriteFunctionToken);
  Writers.Add(TdxSpreadSheetFormulaUnknownFunctionToken, WriteUnknownFunctionToken);
  Writers.Add(TdxSpreadSheetFormulaIntegerValueToken, WriteIntegerToken);
  Writers.Add(TdxSpreadSheetDefinedNameToken, WriteNameToken);
  Writers.Add(TdxSpreadSheetFormulaNullToken, WriteNullToken);
  Writers.Add(TdxSpreadSheetFormulaOperationToken, WriteOperationToken);
  Writers.Add(TdxSpreadSheetFormulaParenthesesToken, WriteParenthesesToken);
  Writers.Add(TdxSpreadSheetFormula3DReference, WriteRef3DToken);
//  Writers.Add(TdxSpreadSheetFormulaReference, WriteRefNToken);
  Writers.Add(TdxSpreadSheetFormulaReference, WriteRefToken);
  Writers.Add(TdxSpreadSheetFormulaStringValueToken, WriteStringToken);
end;

procedure TdxXLSFormulaWriter.WriteAttributeToken(AToken: TdxSpreadSheetFormulaToken);
begin
//  TdxXLSFormulaWriter.
end;

procedure TdxXLSFormulaWriter.WriteArea3DToken(AToken: TdxSpreadSheetFormulaToken);
var
  AData: TObject;
  AIndex, ARow, AColumn, ARow2, AColumn2: Word;
begin
  AData := TdxSpreadSheet3DReferenceLink(TdxSpreadSheetFormula3DAreaReference(AToken).Link).Data;
  if TdxSpreadSheetFormula3DReference(AToken).IsError or TdxSpreadSheetInvalidObject.IsInvalid(AData) then
  begin
    AIndex := 0;
    Writer.WriteByte(ValidateToken(ptgAreaErr3D));
  end
  else
  begin
    AIndex := (AData as TdxSpreadSheetTableView).Index;
    Writer.WriteByte(ValidateToken(ptgArea3D));
  end;
  Writer.WriteWord(AIndex);

  EncodeReference(TdxSpreadSheetFormulaAreaReferenceAccess(AToken).FRow,
    TdxSpreadSheetFormulaAreaReferenceAccess(AToken).FColumn, ARow, AColumn);
  EncodeReference(TdxSpreadSheetFormulaAreaReferenceAccess(AToken).FRow2,
    TdxSpreadSheetFormulaAreaReferenceAccess(AToken).FColumn2, ARow2, AColumn2);
  Writer.WriteWord(ARow);
  Writer.WriteWord(ARow2);
  Writer.WriteWord(AColumn);
  Writer.WriteWord(AColumn2);
end;

procedure TdxXLSFormulaWriter.WriteAreaToken(AToken: TdxSpreadSheetFormulaToken);
var
  ARow, AColumn, ARow2, AColumn2: Word;
begin
  Writer.WriteByte(ValidateToken(ptgArea));
  EncodeReference(TdxSpreadSheetFormulaAreaReferenceAccess(AToken).FRow,
    TdxSpreadSheetFormulaAreaReferenceAccess(AToken).FColumn, ARow, AColumn);
  if TdxSpreadSheetFormulaAreaReferenceAccess(AToken).FColumn.IsAllItems and (AColumn and $FF = $FF) then
    AColumn := AColumn and not $FF;
  EncodeReference(TdxSpreadSheetFormulaAreaReferenceAccess(AToken).FRow2,
    TdxSpreadSheetFormulaAreaReferenceAccess(AToken).FColumn2, ARow2, AColumn2);
  Writer.WriteWord(ARow);
  Writer.WriteWord(ARow2);
  Writer.WriteWord(AColumn);
  Writer.WriteWord(AColumn2);
end;

procedure TdxXLSFormulaWriter.WriteBooleanToken(AToken: TdxSpreadSheetFormulaToken);
begin
  Writer.WriteByte(ptgBool);
  Writer.WriteBoolean(TdxSpreadSheetFormulaBooleanValueToken(AToken).Value);
end;

procedure TdxXLSFormulaWriter.WriteCurrencyToken(AToken: TdxSpreadSheetFormulaToken);
begin
  WriteNumericToken(TdxSpreadSheetFormulaCurrencyValueToken(AToken).Value);
end;

procedure TdxXLSFormulaWriter.WriteDateTimeToken(AToken: TdxSpreadSheetFormulaToken);
begin
  WriteNumericToken(TdxSpreadSheetFormulaDateTimeValueToken(AToken).Value);
end;

procedure TdxXLSFormulaWriter.WriteErrorToken(AToken: TdxSpreadSheetFormulaToken);
begin
  Writer.WriteByte(ptgErr);
  Writer.WriteByte(ErrorCodeToErrorIndex[TdxSpreadSheetFormulaErrorValueToken(AToken).ErrorCode]);
end;

procedure TdxXLSFormulaWriter.WriteFloatToken(AToken: TdxSpreadSheetFormulaToken);
begin
  WriteNumericToken(TdxSpreadSheetFormulaFloatValueToken(AToken).Value);
end;

procedure TdxXLSFormulaWriter.WriteFunctionToken(AToken: TdxSpreadSheetFormulaToken);

  function IsFixedParamsCount(AInfo: TdxSpreadSheetFunctionParamInfo): Boolean;
  var
    I, ACount: Integer;
    AKind: TdxSpreadSheetFunctionParamKindInfo;
  begin
    Result := Assigned(AInfo);
    if not Result then Exit;
    AInfo(ACount, AKind);
    I := 0;
    while Result and (I < ACount) do
    begin
      Result := Result and (AKind[I] in [fpkValue, fpkArray]);
      Inc(I);
    end;
  end;

var
  AFuncID: Word;
begin
  AFuncID := TdxSpreadSheetFormulaFunctionToken(AToken).Information.Token;
  if IsFixedParamsCount(TdxSpreadSheetFormulaFunctionToken(AToken).Information.ParamInfo) then
  begin
    Writer.WriteByte(ValidateToken(ptgFuncV));
    Writer.WriteWord(AFuncID);
  end
  else
  begin
//    Writer.WriteByte(ValidateToken(ptgFuncVar));
    Writer.WriteByte(ptgFuncVarV);
    Writer.WriteByte(AToken.ChildCount and $7F);
//    if AFuncID and $7FFF <> AFuncID then ; // todo: unknown functions for Excel mus be stored as Name
    Writer.WriteWord(AFuncID and $7FFF);
  end;
end;

procedure TdxXLSFormulaWriter.WriteUnknownFunctionToken(AToken: TdxSpreadSheetFormulaToken);
begin
  //
  WriteNameToken(AToken);
  //
  DoWriteTokens(AToken.FirstChild);
  //
  Writer.WriteByte(ptgFuncVarV);
  Writer.WriteByte((AToken.ChildCount + 1) and $7F);
  Writer.WriteWord(255);
end;

procedure TdxXLSFormulaWriter.WriteIntegerToken(AToken: TdxSpreadSheetFormulaToken);
begin
  if InRange(TdxSpreadSheetFormulaIntegerValueToken(AToken).Value, 0, $FFFF) then
  begin
    Writer.WriteByte(ptgInt);
    Writer.WriteWord(TdxSpreadSheetFormulaIntegerValueToken(AToken).Value);
  end
  else
    WriteNumericToken(TdxSpreadSheetFormulaIntegerValueToken(AToken).Value);
end;

procedure TdxXLSFormulaWriter.WriteNameToken(AToken: TdxSpreadSheetFormulaToken);
var
  AName: TdxSpreadSheetDefinedName;
begin
  AName := TdxSpreadSheetDefinedNameToken(AToken).DefinedName;
  if not Formula.IsLinkedToCell then
    Writer.WriteByte(ptgNameA)
  else
    if not (AToken.Parent is TdxSpreadSheetFormulaOperationToken) and
      (AcceptedDataTypes = RefVal) and (AToken.Next = nil) and (AToken.Prev = nil)
    then
      Writer.WriteByte(ptgName)
    else
      Writer.WriteByte(ptgNameV);

  if AName = nil then
    Writer.WriteWord($FFFF)
  else
    Writer.WriteWord(AName.Index + 1);

  Writer.WriteWord(0);
end;

procedure TdxXLSFormulaWriter.WriteNullToken(AToken: TdxSpreadSheetFormulaToken);
begin
  Writer.WriteByte(ptgMissArg);
end;

procedure TdxXLSFormulaWriter.WriteOperationToken(AToken: TdxSpreadSheetFormulaToken);
begin
  Writer.WriteByte(Byte(TdxSpreadSheetFormulaOperationToken(AToken).Operation) + ptgAdd);
end;

procedure TdxXLSFormulaWriter.WriteParenthesesToken(AToken: TdxSpreadSheetFormulaToken);
begin
  Writer.WriteByte(ptgParen);
end;

procedure TdxXLSFormulaWriter.WriteRef3DToken(AToken: TdxSpreadSheetFormulaToken);
var
  AIndex, ARow, AColumn: Word;
  ASheet: TdxSpreadSheetTableView;
const
  ATokens: array[Boolean] of Byte = (ptgRef3d, ptgRefErr3d);
begin
  AIndex := 0;
  ASheet := TdxSpreadSheetTableView(TdxSpreadSheet3DReferenceLink(TdxSpreadSheetFormula3DReference(AToken).Link).Data);
  if (ASheet <> nil) and not TdxSpreadSheetInvalidObject.IsInvalid(ASheet) then
    AIndex := ASheet.Index;
  Writer.WriteByte(ValidateToken(ATokens[TdxSpreadSheetFormula3DReference(AToken).IsError or
    TdxSpreadSheetInvalidObject.IsInvalid(ASheet)]));

//  Writer.WriteByte(ptgRef3dA);

  Writer.WriteWord(AIndex);
  EncodeReference(TdxSpreadSheetFormulaReferenceAccess(AToken).FRow,
    TdxSpreadSheetFormulaReferenceAccess(AToken).FColumn, ARow, AColumn);
  Writer.WriteWord(ARow);
  Writer.WriteWord(AColumn);
end;

procedure TdxXLSFormulaWriter.WriteRefToken(AToken: TdxSpreadSheetFormulaToken);
var
  ARow, AColumn: Word;
const
  ATokens: array[Boolean] of Byte = (ptgRefV, ptgRefErr);
begin
  if TdxSpreadSheetFormulaReferenceAccess(AToken).IsError then
    Writer.WriteByte(ValidateToken(ATokens[TdxSpreadSheetFormulaReferenceAccess(AToken).IsError]))
  else
    Writer.WriteByte(ATokens[TdxSpreadSheetFormulaReferenceAccess(AToken).IsError]);
  EncodeReference(TdxSpreadSheetFormulaReferenceAccess(AToken).FRow,
    TdxSpreadSheetFormulaReferenceAccess(AToken).FColumn, ARow, AColumn);
  Writer.WriteWord(ARow);
  Writer.WriteWord(AColumn);
end;

procedure TdxXLSFormulaWriter.WriteRefNToken(AToken: TdxSpreadSheetFormulaToken);
var
  ARow, AColumn: Integer;
const
  ATokens: array[Boolean] of Byte = (ptgRefN, ptgRefErr);
begin
  if TdxSpreadSheetFormulaReferenceAccess(AToken).IsError then
    WriteRefToken(AToken)
  else
  begin
    Writer.WriteByte(ptgRefNV);
    EncodeReferenceN(TdxSpreadSheetFormulaReferenceAccess(AToken).FRow,
      TdxSpreadSheetFormulaReferenceAccess(AToken).FColumn, ARow, AColumn);
    Writer.WriteSmallInt(ARow);
    Writer.WriteSmallInt(AColumn);
  end;
end;

procedure TdxXLSFormulaWriter.WriteStringToken(AToken: TdxSpreadSheetFormulaToken);
begin
  Writer.WriteByte(ptgStr);
  Writer.XLS_WriteSimpleString(TdxSpreadSheetFormulaStringValueToken(AToken).Value, 1);
end;

procedure TdxXLSFormulaWriter.EncodeReference(ARefRow, ARefColumn: TdxSpreadSheetReference; var ARow, AColumn: Word);
begin
  ARow := Word(-1);
  AColumn := Word(255);
  if not ARefRow.IsAllItems then
    ARow := Word(ARefRow.ActualValue(FAnchorRow));
  if not ARefColumn.IsAllItems then
    AColumn := Word(ARefColumn.ActualValue(FAnchorColumn));
  if not (ARefRow.IsAbsolute or FAbsolute) then
    Word(AColumn) := Word(AColumn) or $8000;
  if not (ARefColumn.IsAbsolute or FAbsolute) then
    Word(AColumn) := Word(AColumn) or $4000;
end;

procedure TdxXLSFormulaWriter.EncodeReferenceN(ARefRow, ARefColumn: TdxSpreadSheetReference; var ARow, AColumn: Integer);
begin
  ARow := Word(-1);
  AColumn := Word(255);
  if not ARefRow.IsAllItems then
    ARow := ARefRow.ActualValue(FAnchorRow) - FAnchorRow;
  if not ARefColumn.IsAllItems then
    AColumn := Byte(Shortint(ARefColumn.ActualValue(FAnchorColumn) - FAnchorColumn)) or $C000;
  if not (ARefRow.IsAbsolute or FAbsolute) then
    AColumn := AColumn or $8000;
  if not (ARefColumn.IsAbsolute or FAbsolute) then
    AColumn := AColumn or $4000;
end;

function TdxXLSFormulaWriter.GetAcceptedDataTypes: TdxXLSFormulaParameterDataTypes;

  function IndexOf(AToken: TdxSpreadSheetFormulaToken): Integer;
  begin
    Result := 0;
    while (AToken <> nil) and (AToken.Prev <> nil) do
    begin
      AToken := AToken.Prev;
      Inc(Result);
    end;
  end;

var
  ATokenIndex: Integer;
  AToken: TdxSpreadSheetFormulaToken;
  AOwnerFormula: TdxSpreadSheetFormulaFunctionToken;
begin
  Result := [fpdtAuto];
  AToken := CurrentToken;
  ATokenIndex := 0;
  while AToken <> nil do
  begin
    if (AToken.Parent <> nil) and (AToken.Parent is TdxSpreadSheetListToken) then
      ATokenIndex := IndexOf(AToken);
    AToken := AToken.Parent;
    if (AToken = nil) or (AToken.ClassType <> TdxSpreadSheetFormulaToken) then
      Break;
  end;
  if not (AToken is TdxSpreadSheetFormulaFunctionToken) then
    Exit;
  AOwnerFormula := AToken as TdxSpreadSheetFormulaFunctionToken;
  Result := Repository.GetFormulaParameterType(
    AOwnerFormula.Information.Token, ATokenIndex);
end;



function TdxXLSFormulaWriter.ValidateToken(AToken: Byte): Byte;
const
  DefaultTypes: array[Boolean] of TdxXLSFormulaParameterDataTypes = ([fpdtReference], [fpdtValue]);
var
  ATypes: TdxXLSFormulaParameterDataTypes;
  ADataType: TdxXLSFormulaParameterDataType;
begin
  ADataType := fpdtReference;
  ATypes := AcceptedDataTypes;
  if ATypes = [fpdtAuto] then
    ATypes := DefaultTypes[Formula.IsLinkedToCell]
  else
    ATypes := [fpdtReference, fpdtArray, fpdtValue] * ATypes;

  if fpdtArray in ATypes then
    ADataType := fpdtArray
  else
    if ATypes = [fpdtValue] then
      ADataType := fpdtValue;

  if (AToken in [ptgName, ptgFuncVar]) and not Formula.IsLinkedToCell { and (AcceptedDataTypes = [fpdtAuto])} then
    ADataType := fpdtArray;
  Result := (AToken and not $60) or (Byte(ADataType) shl 5);

end;

procedure TdxXLSFormulaWriter.WriteNumericToken(const AValue: Double);
begin
  Writer.WriteByte(ptgNum);
  Writer.WriteDateTime(AValue);
end;

{ TdxXLSFunctionsRepository }

constructor TdxXLSFunctionsRepository.Create;
var
  I: Integer;
begin
  FDictionary := TDictionary<Integer, PdxXLSFunctionParametersDefinition>.Create;
  for I := Low(XLSFunctionsDefinition) to High(XLSFunctionsDefinition) do
  begin
    PrepareDefinition(@XLSFunctionsDefinition[I]);
    FDictionary.Add(XLSFunctionsDefinition[I].ID, @XLSFunctionsDefinition[I]);
  end;
end;

destructor TdxXLSFunctionsRepository.Destroy;
begin
  FreeAndNil(FDictionary);
  inherited Destroy;
end;

function TdxXLSFunctionsRepository.GetFormulaParameterType(AID, AIndex: Integer): TdxXLSFormulaParameterDataTypes;
var
  AInfo: PdxXLSFunctionParametersDefinition;
begin
  Result := [fpdtValue];
  if not FDictionary.TryGetValue(AID, AInfo) then
    Exit;
  if AIndex > High(AInfo.Parameters) then
    Result := AInfo.Parameters[High(AInfo.Parameters)]
  else
    Result := AInfo.Parameters[AIndex];
end;

procedure TdxXLSFunctionsRepository.PrepareDefinition(AInfo: PdxXLSFunctionParametersDefinition);
var
  I: Integer;
  AType: TdxXLSFormulaParameterDataTypes;
begin
  AType := AInfo.Parameters[Low(AInfo.Parameters)];
  for I := Low(AInfo.Parameters) + 1 to High(AInfo.Parameters) do
  begin
    if AInfo.Parameters[I] = Same then
      AInfo.Parameters[I] := AType;
    AType := AInfo.Parameters[I];
  end;
end;

initialization
  Repository := TdxXLSFunctionsRepository.Create;

finalization
  FreeAndNil(Repository);

end.





