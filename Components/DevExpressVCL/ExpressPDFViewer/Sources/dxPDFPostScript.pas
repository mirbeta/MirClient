{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPDFViewer                                         }
{                                                                    }
{           Copyright (c) 2015-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPDFVIEWER AND ALL              }
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

unit dxPDFPostScript;

{$I cxVer.inc}

interface

uses
  Classes, SysUtils, Generics.Defaults, Generics.Collections, Windows, cxGeometry, dxPDFBase, dxPDFTypes, dxPDFParser;

type
  TdxPDFPostScriptMark = class(TdxPDFReferencedObject);
  TdxPDFType1FontProgram = class(TdxPDFReferencedObject);

  TdxPDFPostScriptOperator = class;
  TdxPDFPostScriptOperatorClass = class of TdxPDFPostScriptOperator;

  { TdxPDFPostScriptDictionaryEntry }

  TdxPDFPostScriptDictionaryEntry = class(TdxPDFReferencedObject)
  strict private
    FKey: string;
    FValue: TdxPDFReferencedObject;
    procedure SetValue(const AValue: TdxPDFReferencedObject);
  public
    constructor Create(const AKey: string; AValue: TdxPDFReferencedObject);
    destructor Destroy; override;

    property Key: string read FKey;
    property Value: TdxPDFReferencedObject read FValue write SetValue;
  end;

  { TdxPDFPostScriptString }

  TdxPDFPostScriptString = class(TdxPDFString);
  TdxPDFPostScriptOperatorName = class(TdxPDFPostScriptString);

  { TdxPDFPostScriptDictionary }

  TdxPDFPostScriptDictionary = class(TdxPDFReferencedObject)
  strict private
    FItems: TdxPDFReferencedObjects;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): TdxPDFPostScriptDictionaryEntry;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;

    function Contains(AKey: TdxPDFReferencedObject): Boolean;
    function ContainsKey(const AKey: string): Boolean;
    function GetBoolean(const AKey: string): Boolean;
    function GetDouble(const AKey: string; ADefaultValue: Double = 0): Double;
    function GetInteger(const AKey: string; ADefaultValue: Integer = 0): Integer;
    function GetObject(const AKey: string): TdxPDFReferencedObject;
    function GetMatrix(const AKey: string): TXForm;
    function GetRectangle(const AKey: string): TdxRectF;
    function GetString(const AKey: string): string;
    function TryGetValue(const AKey: string; out AValue: TdxPDFReferencedObject): Boolean;
    procedure Add(const AKey: string; AValue: TdxPDFReferencedObject); overload;
    procedure Add(const AKey, AValue: string); overload;
    procedure Add(const AKey: string; AValue: Boolean); overload;
    procedure Add(const AKey: string; AValue: TBytes); overload;
    procedure Add(const AKey: string; AValue: Double); overload;
    procedure Add(const AKey: string; AValue: Integer); overload;
    procedure Add(AKey, AValue: TdxPDFReferencedObject); overload;
    procedure Clear;

    property Items[AIndex: Integer]: TdxPDFPostScriptDictionaryEntry read GetItem; default;
    property Count: Integer read GetCount;
  end;

  { TdxPDFPostScriptStack }

  TdxPDFPostScriptStack = class
  strict private
    FList: TdxPDFReferencedObjects;
    FCount: Integer;
  protected
    property List: TdxPDFReferencedObjects read FList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; inline;
    function PeekAtIndex(AIndex: Integer): TdxPDFReferencedObject;
    function Peek: TdxPDFReferencedObject;
    function Pop: TdxPDFReferencedObject;
    function PopAsDouble: Double;
    procedure Push(AObj: TdxPDFReferencedObject);
    procedure Exchange; inline;

    property Count: Integer read FCount;
  end;

  { TdxPDFPostScriptFileParser }

  TdxPDFPostScriptFileParser = class(TdxPDFBaseParser)
  strict private const
  {$REGION 'internal const'}
    BeginProcedure = Byte('{');
    EndProcedure = Byte('}');
    One = Byte('1');
    RadixNumberIdentifier = Byte('#');
    Seven = Byte('7');
    Zero = Byte('0');
  {$ENDREGION}
  strict private
    FClosed: Boolean;
    FOperators: TdxPDFReferencedObjects;
    FShouldExpectClosing: Boolean;
    function ReadRadixNumber(AMode: Integer): TdxPDFNumericObject;
  protected
    function CanContinueReading: Boolean; override;
    function CreateDefaultCompositeObject: TdxPDFBase; override;
    function DoReadString: TdxPDFString; override;
    function DoSkipSpaces: Boolean; override;
    function NeedReadObjectAfterComment: Boolean; override;
    function ReadCompositeObject(AIsHexStrSeparatedByWhiteSpaces: Boolean = False): TdxPDFBase; override;
    function ReadNumericObject: TdxPDFBase; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Read(const AData: TBytes; APosition: Integer = 0): TdxPDFReferencedObjects;
    function ReadNextObject: TdxPDFReferencedObject;
    function ReadString(var S: TBytes): Integer;
    property Closed: Boolean read FClosed;
  end;

  { TdxPDFPostScriptInterpreter }

  TdxPDFPostScriptInterpreter = class
  strict private
    FDictionaryStack: TdxPDFPostScriptStack;
    FFileIsClosed: Boolean;
    FFontDirectory: TdxPDFPostScriptDictionary;
    FParser: TdxPDFPostScriptFileParser;
    FStack: TdxPDFPostScriptStack;
    FSystemDictionary: TdxPDFPostScriptDictionary;
    FUserDictionary: TdxPDFPostScriptDictionary;
    procedure SetParser(const AValue: TdxPDFPostScriptFileParser);
  protected
    procedure CloseFile;
    procedure InternalExecute(AObj: TdxPDFReferencedObject); overload;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute(AArray: TdxPDFArray); overload;
    procedure Execute(AOperators: TdxPDFReferencedObjects); overload;
    procedure Execute(const AData: TBytes); overload;

    property DictionaryStack: TdxPDFPostScriptStack read FDictionaryStack;
    property FontDirectory: TdxPDFPostScriptDictionary read FFontDirectory;
    property Parser: TdxPDFPostScriptFileParser read FParser write SetParser;
    property Stack: TdxPDFPostScriptStack read FStack;
    property SystemDictionary: TdxPDFPostScriptDictionary read FSystemDictionary;
    property UserDictionary: TdxPDFPostScriptDictionary read FUserDictionary;
  end;

  { TdxPDFPostScriptOperator }

  TdxPDFPostScriptOperator = class(TdxPDFBase)
  protected
    class function GetName: string; virtual;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); virtual; abstract;
  end;

  { TdxPDFPostScriptAbsOperator }

  TdxPDFPostScriptAbsOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptAddOperator }

  TdxPDFPostScriptAddOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptStringOperator }

  TdxPDFPostScriptStringOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptComment }

  TdxPDFPostScriptComment = class(TdxPDFPostScriptOperator)
  private
    FText: string;
  public
    constructor Create(const AText: string);
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;

    property Text: string read FText;
  end;

  { TdxPDFPostScriptArrayOperator }

  TdxPDFPostScriptArrayOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptBeginOperator }

  TdxPDFPostScriptBeginOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptCloseFileOperator }

  TdxPDFPostScriptCloseFileOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptCurrentDictionaryOperator }

  TdxPDFPostScriptCurrentDictionaryOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptCurrentFileOperator }

  TdxPDFPostScriptCurrentFileOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptCvrOperator }

  TdxPDFPostScriptCvrOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptDefineFontOperator }

  TdxPDFPostScriptDefineFontOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptIndexOperator }

  TdxPDFPostScriptIndexOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptRollOperator }

  TdxPDFPostScriptRollOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptSubOperator }

  TdxPDFPostScriptSubOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptExchangeOperator }

  TdxPDFPostScriptExchangeOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptPopOperator }

  TdxPDFPostScriptPopOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptDupOperator }

  TdxPDFPostScriptDupOperator = class(TdxPDFPostScriptComment)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptMulOperator }

  TdxPDFPostScriptMulOperator = class(TdxPDFPostScriptComment)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptDictOperator }

  TdxPDFPostScriptDictOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptDefOperator }

  TdxPDFPostScriptDefOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptReadonlyOperator }

  TdxPDFPostScriptReadonlyOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptPutOperator }

  TdxPDFPostScriptPutOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptForOperator }

  TdxPDFPostScriptForOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptEndOperator }

  TdxPDFPostScriptEndOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptEexecOperator }

  TdxPDFPostScriptEexecOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptReadStringOperator }

  TdxPDFPostScriptReadStringOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptExecuteOnlyOperator }

  TdxPDFPostScriptExecuteOnlyOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptNoAccessOperator }

  TdxPDFPostScriptNoAccessOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptType1FontCustomOperator }

  TdxPDFPostScriptType1FontCustomOperator = class(TdxPDFPostScriptOperator)
  private
    FTokens: array of string;
    function DoExecute(AInterpreter: TdxPDFPostScriptInterpreter; const AToken: string): Boolean; overload;
    function DoExecute(AInterpreter: TdxPDFPostScriptInterpreter; ADictionary: TdxPDFPostScriptDictionary;
      const AToken: string): Boolean; overload;
  protected
    procedure InitializeTokens; virtual; abstract;
  public
    constructor Create; override;
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptType1FontRDOperator }

  TdxPDFPostScriptType1FontRDOperator = class(TdxPDFPostScriptType1FontCustomOperator)
  protected
    class function GetName: string; override;
    procedure InitializeTokens; override;
  end;

  { TdxPDFPostScriptType1FontRDAlternativeOperator }

  TdxPDFPostScriptType1FontRDAlternativeOperator = class(TdxPDFPostScriptType1FontRDOperator)
  protected
    class function GetName: string; override;
  end;

  { TdxPDFPostScriptType1FontNDOperator }

  TdxPDFPostScriptType1FontNDOperator = class(TdxPDFPostScriptType1FontCustomOperator)
  protected
    class function GetName: string; override;
    procedure InitializeTokens; override;
  end;

  { TdxPDFPostScriptType1FontNDAlternativeOperator }

  TdxPDFPostScriptType1FontNDAlternativeOperator = class(TdxPDFPostScriptType1FontNDOperator)
  protected
    class function GetName: string; override;
  end;

  { TdxPDFPostScriptType1FontNPOperator }

  TdxPDFPostScriptType1FontNPOperator = class(TdxPDFPostScriptType1FontCustomOperator)
  protected
    class function GetName: string; override;
    procedure InitializeTokens; override;
  end;

  { TdxPDFPostScriptType1FontNPAlternativeOperator }

  TdxPDFPostScriptType1FontNPAlternativeOperator = class(TdxPDFPostScriptType1FontNPOperator)
  protected
    class function GetName: string; override;
  end;

  { TdxPDFPostScriptGetOperator }

  TdxPDFPostScriptGetOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptKnownOperator }

  TdxPDFPostScriptKnownOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptMarkOperator }

  TdxPDFPostScriptMarkOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptFindFontOperator }

  TdxPDFPostScriptFindFontOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptFontDirectoryOperator }

  TdxPDFPostScriptFontDirectoryOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptGeOperator }

  TdxPDFPostScriptGeOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptGtOperator }

  TdxPDFPostScriptGtOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptIfElseOperator }

  TdxPDFPostScriptIfElseOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptIfOperator }

  TdxPDFPostScriptIfOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptLeOperator }

  TdxPDFPostScriptLeOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptLtOperator }

  TdxPDFPostScriptLtOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptStandardEncodingOperator }

  TdxPDFPostScriptStandardEncodingOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptSystemDictOperator }

  TdxPDFPostScriptSystemDictOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptUserDictOperator }

  TdxPDFPostScriptUserDictOperator = class(TdxPDFPostScriptOperator)
  protected
    class function GetName: string; override;
  public
    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

  { TdxPDFPostScriptGetDictionaryElementOperator }

  TdxPDFPostScriptGetDictionaryElementOperator = class(TdxPDFPostScriptOperator)
  strict private
    FKey: string;
  public
    constructor Create(const AKey: string); reintroduce;

    procedure Execute(AInterpreter: TdxPDFPostScriptInterpreter); override;
  end;

implementation

uses
  Math, Variants, dxFontFile, dxPDFType1Font, dxPDFUtils;

type
  TdxPDFNumericObjectAccess = class(TdxPDFNumericObject);

  { TdxPDFPostScriptOperatorFactory }

  TdxPDFPostScriptOperatorFactory = class(TdxPDFFactory<TdxPDFPostScriptOperatorClass>)
  public
    procedure RegisterOperator(AOperatorClass: TdxPDFPostScriptOperatorClass);
    procedure UnregisterOperator(AOperatorClass: TdxPDFPostScriptOperatorClass);
  end;

var
  dxgPostScriptOperatorFactory: TdxPDFPostScriptOperatorFactory;

function dxPDFPostScriptOperatorFactory: TdxPDFPostScriptOperatorFactory;
begin
  if dxgPostScriptOperatorFactory = nil then
    dxgPostScriptOperatorFactory := TdxPDFPostScriptOperatorFactory.Create;
  Result := dxgPostScriptOperatorFactory;
end;

{ TdxPDFPostScriptOperatorFactory }

procedure TdxPDFPostScriptOperatorFactory.RegisterOperator(AOperatorClass: TdxPDFPostScriptOperatorClass);
begin
  if not ContainsKey(AOperatorClass.GetName) then
    Register(AOperatorClass.GetName, AOperatorClass)
end;

procedure TdxPDFPostScriptOperatorFactory.UnregisterOperator(AOperatorClass: TdxPDFPostScriptOperatorClass);
begin
  UnregisterClass(AOperatorClass.GetName);
end;

{ TdxPDFPostScriptDictionaryEntry }

constructor TdxPDFPostScriptDictionaryEntry.Create(const AKey: string; AValue: TdxPDFReferencedObject);
begin
  inherited Create;
  FKey := AKey;
  Value := AValue;
end;

destructor TdxPDFPostScriptDictionaryEntry.Destroy;
begin
  Value := nil;
  inherited Destroy;
end;

procedure TdxPDFPostScriptDictionaryEntry.SetValue(const AValue: TdxPDFReferencedObject);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FValue));
end;

{ TdxPDFPostScriptDictionary }

constructor TdxPDFPostScriptDictionary.Create(ACapacity: Integer);
begin
  inherited Create;
  FItems := TdxPDFReferencedObjects.Create;
  FItems.Capacity := ACapacity;
end;

destructor TdxPDFPostScriptDictionary.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TdxPDFPostScriptDictionary.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxPDFPostScriptDictionary.GetItem(AIndex: Integer): TdxPDFPostScriptDictionaryEntry;
begin
  Result := FItems[AIndex] as TdxPDFPostScriptDictionaryEntry;
end;

function TdxPDFPostScriptDictionary.ContainsKey(const AKey: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    Result := (FItems[I] as TdxPDFPostScriptDictionaryEntry).Key = AKey;
    if Result then
      Break;
  end;
end;

function TdxPDFPostScriptDictionary.GetBoolean(const AKey: string): Boolean;
var
  AObject: TdxPDFReferencedObject;
begin
  Result := False;
  if TryGetValue(AKey, AObject) then
    if AObject is TdxPDFBoolean then
      Result := TdxPDFBoolean(AObject).Value;
end;

function TdxPDFPostScriptDictionary.GetDouble(const AKey: string; ADefaultValue: Double = 0): Double;
var
  AObject: TdxPDFReferencedObject;
begin
  Result := ADefaultValue;
  if TryGetValue(AKey, AObject) then
    if AObject is TdxPDFDouble then
      Result := TdxPDFDouble(AObject).Value;
end;

function TdxPDFPostScriptDictionary.GetInteger(const AKey: string; ADefaultValue: Integer = 0): Integer;
var
  AObject: TdxPDFReferencedObject;
begin
  Result := ADefaultValue;
  if TryGetValue(AKey, AObject) then
    if AObject is TdxPDFInteger then
      Result := TdxPDFInteger(AObject).Value;
end;

function TdxPDFPostScriptDictionary.GetObject(const AKey: string): TdxPDFReferencedObject;
begin
  if not TryGetValue(AKey, Result) then
    Result := nil;
end;

function TdxPDFPostScriptDictionary.GetMatrix(const AKey: string): TXForm;
var
  AMatrix: TdxPDFTransformationMatrix;
begin
  AMatrix := TdxPDFUtils.ArrayToMatrix(GetObject(AKey) as TdxPDFArray);
  try
    Result.eM11 := AMatrix.A;
    Result.eM12 := AMatrix.B;
    Result.eM21 := AMatrix.C;
    Result.eM22 := AMatrix.D;
    Result.eDx := AMatrix.E;
    Result.eDy := AMatrix.F;
  finally
    AMatrix.Free;
  end;
end;

function TdxPDFPostScriptDictionary.GetRectangle(const AKey: string): TdxRectF;
begin
  Result := TdxPDFUtils.ArrayToRectF(GetObject(AKey) as TdxPDFArray);
end;

function TdxPDFPostScriptDictionary.GetString(const AKey: string): string;
var
  AObject: TdxPDFReferencedObject;
begin
  Result := '';
  if TryGetValue(AKey, AObject) then
    if AObject is TdxPDFString then
      Result := TdxPDFString(AObject).Value;
end;

function TdxPDFPostScriptDictionary.TryGetValue(const AKey: string; out AValue: TdxPDFReferencedObject): Boolean;
var
  I: Integer;
  AEntry: TdxPDFPostScriptDictionaryEntry;
begin
  AValue := nil;
  Result := False;
  for I := 0 to Count - 1 do
  begin
    AEntry := FItems[I] as TdxPDFPostScriptDictionaryEntry;
    Result := AEntry.Key = AKey;
    if Result then
    begin
      AValue := AEntry.Value;
      Break;
    end;
  end;
end;

function TdxPDFPostScriptDictionary.Contains(AKey: TdxPDFReferencedObject): Boolean;
begin
  Result := AKey is TdxPDFString;
  if Result then
    Result := ContainsKey(TdxPDFString(AKey).Value);
end;

procedure TdxPDFPostScriptDictionary.Add(const AKey: string; AValue: TdxPDFReferencedObject);
begin
  FItems.Add(TdxPDFPostScriptDictionaryEntry.Create(AKey, AValue));
end;

procedure TdxPDFPostScriptDictionary.Add(const AKey, AValue: string);
begin
  Add(AKey, TdxPDFString.Create(AValue));
end;

procedure TdxPDFPostScriptDictionary.Add(const AKey: string; AValue: Boolean);
begin
  Add(AKey, TdxPDFBoolean.Create(AValue));
end;

procedure TdxPDFPostScriptDictionary.Add(const AKey: string; AValue: TBytes);
begin
  Add(AKey, TdxPDFString.Create(TdxPDFUtils.ConvertToStr(AValue)));
end;

procedure TdxPDFPostScriptDictionary.Add(const AKey: string; AValue: Double);
begin
  Add(AKey, TdxPDFDouble.Create(AValue));
end;

procedure TdxPDFPostScriptDictionary.Add(const AKey: string; AValue: Integer);
begin
  Add(AKey, TdxPDFInteger.Create(AValue));
end;

procedure TdxPDFPostScriptDictionary.Add(AKey, AValue: TdxPDFReferencedObject);
begin
  if AKey is TdxPDFString then
    Add(TdxPDFString(AKey).Value, AValue);
end;

procedure TdxPDFPostScriptDictionary.Clear;
begin
  FItems.Clear;
end;

{ TdxPDFPostScriptStack }

constructor TdxPDFPostScriptStack.Create;
begin
  inherited Create;
  FList := TdxPDFReferencedObjects.Create;
end;

destructor TdxPDFPostScriptStack.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TdxPDFPostScriptStack.Clear;
begin
  FCount := 0;
  FList.Clear;
end;

function TdxPDFPostScriptStack.PeekAtIndex(AIndex: Integer): TdxPDFReferencedObject;
begin
  if (AIndex >= 0) and (AIndex < FCount) then
    Result := FList[FCount - AIndex - 1]
  else
    Result := nil;
end;

function TdxPDFPostScriptStack.Peek: TdxPDFReferencedObject;
begin
  Result := PeekAtIndex(0);
end;

function TdxPDFPostScriptStack.Pop: TdxPDFReferencedObject;
begin
  if FCount > 0 then
  begin
    Dec(FCount);
    Result := FList[FCount];
  end
  else
    Result := nil;
end;

function TdxPDFPostScriptStack.PopAsDouble: Double;
begin
  Result := TdxPDFUtils.ConvertToDouble(Pop as TdxPDFBase);
end;

procedure TdxPDFPostScriptStack.Push(AObj: TdxPDFReferencedObject);
begin
  if FCount < FList.Count then
    FList[FCount] := AObj
  else
    FList.Add(AObj);
  Inc(FCount);
end;

procedure TdxPDFPostScriptStack.Exchange;
begin
  if FCount >= 2 then
    FList.Exchange(FCount - 1, FCount - 2);
end;

{ TdxPDFPostScriptInterpreter }

constructor TdxPDFPostScriptInterpreter.Create;
begin
  inherited Create;
  Parser := nil;
  FStack := TdxPDFPostScriptStack.Create;
  FDictionaryStack := TdxPDFPostScriptStack.Create;
  FFontDirectory := TdxPDFPostScriptDictionary.Create(0);
  FFontDirectory.Reference;
  FSystemDictionary := TdxPDFPostScriptDictionary.Create(0);
  FSystemDictionary.Reference;
  FUserDictionary := TdxPDFPostScriptDictionary.Create(0);
  FUserDictionary.Reference;
end;

destructor TdxPDFPostScriptInterpreter.Destroy;
begin
  Parser := nil;
  FreeAndNil(FUserDictionary);
  FreeAndNil(FSystemDictionary);
  FreeAndNil(FFontDirectory);
  FreeAndNil(FDictionaryStack);
  FreeAndNil(FStack);
  inherited Destroy;
end;

procedure TdxPDFPostScriptInterpreter.Execute(AArray: TdxPDFArray);
var
  AOperators: TdxPDFReferencedObjects;
  AValue: TdxPDFBase;
begin
  AOperators := TdxPDFReferencedObjects.Create;
  try
    for AValue in AArray.ElementList do
      if AValue is TdxPDFReferencedObject then
        AOperators.Add(TdxPDFReferencedObject(AValue));
    Execute(AOperators);
  finally
    AOperators.Free;
  end;
end;

procedure TdxPDFPostScriptInterpreter.Execute(AOperators: TdxPDFReferencedObjects);
var
  AObj: TdxPDFReferencedObject;
begin
  for AObj in AOperators do
    InternalExecute(AObj);
end;

procedure TdxPDFPostScriptInterpreter.Execute(const AData: TBytes);
var
  AObj: TdxPDFReferencedObject;
  AParser: TdxPDFPostScriptFileParser;
begin
  AParser := TdxPDFPostScriptFileParser.Create(AData, 0);
  Parser := AParser;
  while not FFileIsClosed do
  begin
    AObj := AParser.ReadNextObject as TdxPDFReferencedObject;
    if AObj = nil then
      Break;
    AObj.Reference;
    InternalExecute(AObj);
    dxPDFFreeObject(AObj);
  end;
end;

procedure TdxPDFPostScriptInterpreter.CloseFile;
begin
  FFileIsClosed := True;
end;

procedure TdxPDFPostScriptInterpreter.InternalExecute(AObj: TdxPDFReferencedObject);
begin
  if AObj is TdxPDFPostScriptOperator then
    TdxPDFPostScriptOperator(AObj).Execute(Self)
  else
    FStack.Push(AObj)
end;

procedure TdxPDFPostScriptInterpreter.SetParser(const AValue: TdxPDFPostScriptFileParser);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FParser));
end;

{ TdxPDFPostScriptFileParser }

constructor TdxPDFPostScriptFileParser.Create;
begin
  inherited Create;
  FOperators := TdxPDFReferencedObjects.Create;
  FShouldExpectClosing := CurrentPosition <> 0;
end;

destructor TdxPDFPostScriptFileParser.Destroy;
begin
  FreeAndNil(FOperators);
  inherited Destroy;
end;

function TdxPDFPostScriptFileParser.Read(const AData: TBytes; APosition: Integer = 0): TdxPDFReferencedObjects;
var
  AObj: TdxPDFReferencedObject;
begin
  Result := TdxPDFReferencedObjects.Create;
  Data := AData;
  CurrentPosition := APosition;
  FShouldExpectClosing := CurrentPosition <> 0;
  while True do
  begin
    AObj := ReadNextObject;
    if AObj = nil then
      Break;
    Result.Add(AObj);
  end;
  if FShouldExpectClosing and not FClosed then
    TdxPDFUtils.RaiseTestException(ClassName + ': Read');
end;

function TdxPDFPostScriptFileParser.CanContinueReading: Boolean;
begin
  Result := not TdxPDFUtils.IsSpace(Current) and not TdxPDFUtils.IsWhiteSpace(Current) and
    (Current <> TdxPDFDefinedSymbols.Comment) and (Current <> TdxPDFDefinedSymbols.NameIdentifier) and
    (Current <> TdxPDFDefinedSymbols.StartString) and (Current <> TdxPDFDefinedSymbols.EndString) and
    (Current <> TdxPDFDefinedSymbols.StartArray) and (Current <> TdxPDFDefinedSymbols.EndArray) and
    (Current <> BeginProcedure) and (Current <> EndProcedure);
end;

function TdxPDFPostScriptFileParser.CreateDefaultCompositeObject: TdxPDFBase;
begin
  Result := TdxPDFPostScriptOperatorName.Create(DoReadToken);
end;

function TdxPDFPostScriptFileParser.DoReadString: TdxPDFString;
var
  ATemp: TdxPDFString;
begin
  ATemp := inherited DoReadString;
  try
    Result := TdxPDFPostScriptString.Create(ATemp.Value);
  finally
     ATemp.Free;
  end;
end;

function TdxPDFPostScriptFileParser.DoSkipSpaces: Boolean;
begin
  Result := SkipSpaces(False);
end;

function TdxPDFPostScriptFileParser.NeedReadObjectAfterComment: Boolean;
begin
  Result := False;
end;

function TdxPDFPostScriptFileParser.ReadNextObject: TdxPDFReferencedObject;
var
  AObject: TdxPDFReferencedObject;
  AClass: TdxPDFPostScriptOperatorClass;
begin
  Result := nil;
  AObject := DoReadObject;
  if AObject <> nil then
  begin
    if AObject is TdxPDFPostScriptStringOperator then
      Exit(AObject);
    if AObject is TdxPDFComment then
    begin
      Result := TdxPDFPostScriptComment.Create(TdxPDFComment(AObject).Value);
      dxPDFFreeObject(AObject);
      Exit;
    end;
    if AObject is TdxPDFName then
      Exit(AObject);
    if AObject is TdxPDFPostScriptOperatorName then
    begin
      if dxPDFPostScriptOperatorFactory.TryGetClass(TdxPDFString(AObject).Value, AClass) then
        Result := AClass.Create
      else
        Result := TdxPDFPostScriptGetDictionaryElementOperator.Create(TdxPDFString(AObject).Value);
      dxPDFFreeObject(AObject);
      Exit;
    end;
    Result := AObject;
  end;
end;

function TdxPDFPostScriptFileParser.ReadString(var S: TBytes): Integer;
var
  ALength: Integer;
begin
  if not IsWhiteSpace or not ReadNext then
    TdxPDFUtils.RaiseTestException;
  ALength := Length(S);
  S := Copy(Data, CurrentPosition, ALength);
  if CurrentPosition + ALength > DataLength then
    ALength := DataLength - CurrentPosition - ALength;
  CurrentPosition := CurrentPosition + ALength;
  Result := ALength;
end;

function TdxPDFPostScriptFileParser.ReadNumericObject: TdxPDFBase;
var
  ACurrent: Byte;
  AValue: TObject;
begin
  if Current = TdxPDFDefinedSymbols.Minus then
  begin
    if not ReadNext then
      TdxPDFUtils.RaiseTestException;
    ACurrent := Current;
    ReadPrev;
    if ACurrent = Byte('|') then
      Exit(nil);
  end;
  AValue := inherited ReadNumericObject;
  if (Current = RadixNumberIdentifier) and (AValue is TdxPDFInteger) then
  begin
    Result := ReadRadixNumber(TdxPDFInteger(AValue).Value) as TdxPDFBase;
    dxPDFFreeObject(TdxPDFBase(AValue));
  end
  else
    Result := AValue as TdxPDFBase;
end;

function TdxPDFPostScriptFileParser.ReadRadixNumber(AMode: Integer): TdxPDFNumericObject;
var
  AIsValidByte: Boolean;
begin
  Result := TdxPDFInteger.Create;
  while ReadNext do
  begin
    case AMode of
      2:
        AIsValidByte := (current = Byte('0')) or (current = Byte('1'));
      8:
        AIsValidByte := (current >= Byte('0')) and (current <= Byte('7'));
      16:
        AIsValidByte := TdxPDFUtils.IsHexDigit(Current);
    else
      AIsValidByte := False;
    end;
    if not AIsValidByte then
      Break;
    case AMode of
      2:
        TdxPDFInteger(Result).Value := TdxPDFInteger(Result).Value * 2 + TdxPDFUtils.ConvertToDigit(Current);
      8:
        TdxPDFInteger(Result).Value := TdxPDFInteger(Result).Value * 8 + TdxPDFUtils.ConvertToDigit(Current);
      16:
        TdxPDFInteger(Result).Value := TdxPDFInteger(Result).Value * 16 + TdxPDFUtils.ByteToHexDigit(Current)
    end;
  end;
end;

function TdxPDFPostScriptFileParser.ReadCompositeObject(AIsHexStrSeparatedByWhiteSpaces: Boolean = False): TdxPDFBase;
var
  I: Integer;
  AList: TdxPDFReferencedObjects;
  AArray: TdxPDFArray;
  AParser: TdxPDFPostScriptFileParser;
begin
  case Current of
    BeginProcedure:
      begin
        AParser := TdxPDFPostScriptFileParser.Create;
        try
          AList := AParser.Read(Data, CurrentPosition + 1);
          try
            AArray := TdxPDFArray.Create;
            for I := 0 to AList.Count - 1 do
              AArray.Add(AList[I] as TdxPDFBase);
            CurrentPosition := AParser.CurrentPosition + 1;
            Result := AArray;
          finally
            AList.Free;
          end;
        finally
          AParser.Free;
        end;
      end;
    EndProcedure:
      begin
        if not FShouldExpectClosing then
          TdxPDFUtils.RaiseTestException;
        FClosed := True;
        Result := nil;
      end;
  else
    Result := inherited ReadCompositeObject(AIsHexStrSeparatedByWhiteSpaces);
  end;
end;

class function TdxPDFPostScriptOperator.GetName: string;
begin
  Result := '';
end;

{ TdxPDFPostScriptStringOperator }

procedure TdxPDFPostScriptStringOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
var
  AStack: TdxPDFPostScriptStack;
  AOperand: TObject;
  S: string;
begin
  AStack := AInterpreter.Stack;
  AOperand := AStack.Pop;
  if not (AOperand is TdxPDFInteger) then
    TdxPDFUtils.RaiseTestException('TdxPDFPostScriptStringOperator.Execute');
  if TdxPDFInteger(AOperand).Value < 0 then
    TdxPDFUtils.RaiseTestException('TdxPDFPostScriptStringOperator.Execute');
  SetLength(S, TdxPDFInteger(AOperand).Value);
  AStack.Push(TdxPDFString.Create(S));
end;

class function TdxPDFPostScriptStringOperator.GetName: string;
begin
  Result := 'string';
end;

{ TdxPDFPostScriptComment }

constructor TdxPDFPostScriptComment.Create(const AText: string);
begin
  inherited Create;
  FText := AText;
end;

procedure TdxPDFPostScriptComment.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
begin
// do nothing
end;

{ TdxPDFPostScriptAbsOperator }

procedure TdxPDFPostScriptAbsOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
var
  AOperand: TObject;
  AValue: Integer;
begin
  AOperand := AInterpreter.Stack.Pop;
  if AOperand is TdxPDFInteger then
  begin
    AValue := TdxPDFInteger(AOperand).Value;
    if AValue = -MaxInt then
      AInterpreter.Stack.Push(TdxPDFDouble.Create(AValue))
    else
      AInterpreter.Stack.Push(TdxPDFDouble.Create(Abs(AValue)));
  end
  else
    AInterpreter.Stack.Push(TdxPDFDouble.Create(Abs(TdxPDFUtils.ConvertToDouble(AOperand as TdxPDFBase))));
end;

class function TdxPDFPostScriptAbsOperator.GetName: string;
begin
  Result := 'abs';
end;

{ TdxPDFPostScriptAddOperator }

procedure TdxPDFPostScriptAddOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
var
  AOperand1, AOperand2: TdxPDFBase;
begin
  AOperand1 := AInterpreter.Stack.Pop as TdxPDFBase;
  AOperand2 := AInterpreter.Stack.Pop as TdxPDFBase;
  if (AOperand1.ObjectType = otInteger) and (AOperand2.ObjectType = otInteger) then
    AInterpreter.Stack.Push(TdxPDFDouble.Create(TdxPDFInteger(AOperand1).Value + TdxPDFInteger(AOperand2).Value))
  else
    AInterpreter.Stack.Push(TdxPDFDouble.Create(TdxPDFUtils.ConvertToDouble(AOperand1) + TdxPDFUtils.ConvertToDouble(AOperand2)));
end;

class function TdxPDFPostScriptAddOperator.GetName: string;
begin
  Result := 'add';
end;

{ TdxPDFPostScriptArrayOperator }

procedure TdxPDFPostScriptArrayOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
var
  I: Integer;
  AValue: TObject;
  AArray: TdxPDFArray;
begin
  AValue := AInterpreter.Stack.Pop;
  if not (AValue is TdxPDFInteger) then
    TdxPDFUtils.RaiseTestException('TdxPDFPostScriptArrayOperator.Execute');
  if TdxPDFInteger(AValue).Value < 0 then
    TdxPDFUtils.RaiseTestException('TdxPDFPostScriptArrayOperator.Execute');
  AArray := TdxPDFArray.Create;
  for I := 0 to TdxPDFInteger(AValue).Value - 1 do
    AArray.Add(TdxPDFInteger.Create);
  AInterpreter.Stack.Push(AArray);
end;

class function TdxPDFPostScriptArrayOperator.GetName: string;
begin
  Result := 'array';
end;

{ TdxPDFPostScriptBeginOperator }

procedure TdxPDFPostScriptBeginOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
var
  AObject: TdxPDFReferencedObject;
begin
  AObject := AInterpreter.Stack.Pop;
  if AObject is TdxPDFPostScriptDictionary then
    AInterpreter.DictionaryStack.Push(AObject)
  else
    TdxPDFUtils.RaiseTestException('TdxPDFPostScriptBeginOperator.Execute');
end;

class function TdxPDFPostScriptBeginOperator.GetName: string;
begin
  Result := 'begin';
end;

{ TdxPDFPostScriptCloseFileOperator }

procedure TdxPDFPostScriptCloseFileOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
var
  AParser: TdxPDFPostScriptFileParser;
begin
  AParser := AInterpreter.Stack.Pop as TdxPDFPostScriptFileParser;
  if (AParser = nil) or (AParser <> AInterpreter.Parser) then
    TdxPDFUtils.RaiseTestException('TdxPDFPostScriptCloseFileOperator.Execute');
  AInterpreter.CloseFile;
end;

class function TdxPDFPostScriptCloseFileOperator.GetName: string;
begin
  Result := 'closefile'
end;

{ TdxPDFPostScriptCurrentDictionaryOperator }

procedure TdxPDFPostScriptCurrentDictionaryOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
begin
  if AInterpreter.DictionaryStack.Count <= 0 then
    TdxPDFUtils.RaiseTestException('TdxPDFPostScriptCurrentDictionaryOperator.Execute');
 AInterpreter.Stack.Push(AInterpreter.DictionaryStack.Peek);
end;

class function TdxPDFPostScriptCurrentDictionaryOperator.GetName: string;
begin
  Result := 'currentdict';
end;

{ TdxPDFPostScriptCurrentFileOperator }

procedure TdxPDFPostScriptCurrentFileOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
begin
  if AInterpreter.Parser <> nil then
    AInterpreter.Stack.Push(AInterpreter.Parser);
end;

class function TdxPDFPostScriptCurrentFileOperator.GetName: string;
begin
  Result := 'currentfile';
end;

{ TdxPDFPostScriptCvrOperator }

procedure TdxPDFPostScriptCvrOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
begin
  AInterpreter.Stack.Push(
    TdxPDFDouble.Create(TdxPDFUtils.ConvertToDouble(AInterpreter.Stack.Pop as TdxPDFBase)));
end;

class function TdxPDFPostScriptCvrOperator.GetName: string;
begin
  Result := 'cvr';
end;

{ TdxPDFPostScriptDefineFontOperator }

procedure TdxPDFPostScriptDefineFontOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
var
  ADictionary: TdxPDFPostScriptDictionary;
  AKey: TdxPDFString;
  AProgram: TdxPDFType1FontClassicFontProgram;
begin
  ADictionary := AInterpreter.Stack.Pop as TdxPDFPostScriptDictionary;
  AKey := AInterpreter.Stack.Pop as TdxPDFString;
  if (ADictionary = nil) or (AKey = nil) then
    TdxPDFUtils.RaiseTestException('TdxPDFPostScriptDefineFontOperator.Execute');
  AProgram := TdxPDFType1FontClassicFontProgram.Create;
  AProgram.Read(ADictionary);
  AInterpreter.FontDirectory.Add(AKey, AProgram);
  AInterpreter.Stack.Push(AProgram);
end;

class function TdxPDFPostScriptDefineFontOperator.GetName: string;
begin
  Result := 'definefont';
end;

{ TdxPDFPostScriptIndexOperator }

procedure TdxPDFPostScriptIndexOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
var
  AValue: TdxPDFReferencedObject;
begin
  AValue := AInterpreter.Stack.Pop;
  if not (AValue is TdxPDFNumericObject) then
    TdxPDFUtils.RaiseTestException('TdxPDFPostScriptIndexOperator.Execute(');
  AInterpreter.Stack.Push(AInterpreter.Stack.PeekAtIndex(TdxPDFNumericObjectAccess(AValue).InternalValue));
end;

class function TdxPDFPostScriptIndexOperator.GetName: string;
begin
  Result := 'index';
end;

{ TdxPDFPostScriptRollOperator }

procedure TdxPDFPostScriptRollOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
var
  AShiftObject, ACountObject: TObject;
  ACount, AShift, I, ALastIndex: Integer;
  AList: TdxPDFReferencedObjects;
begin
  AShiftObject := AInterpreter.Stack.Pop;
  ACountObject := AInterpreter.Stack.Pop;
  if (not (AShiftObject is TdxPDFNumericObject)) or (not (ACountObject is TdxPDFNumericObject)) then
    TdxPDFUtils.RaiseTestException('TdxPDFPostScriptRollOperator.Execute');
  ACount := TdxPDFNumericObjectAccess(ACountObject).GetValue;
  AShift := TdxPDFNumericObjectAccess(AShiftObject).GetValue;
  AList := TdxPDFReferencedObjects.Create;
  try
    for I := 0 to ACount - 1 do
      AList.Add(AInterpreter.Stack.Pop);
    Dec(AShift, AShift div ACount * ACount);
    ALastIndex := ACount - 1;
    Inc(AShift, ALastIndex);
    if AShift >= ACount then
      Dec(AShift, ACount);
    for I := 0 to ACount - 1 do
    begin
      AInterpreter.Stack.Push(AList[AShift]);
      Dec(AShift);
      if AShift < 0 then
        AShift := ALastIndex;
    end;
  finally
    AList.Free;
  end;
end;

class function TdxPDFPostScriptRollOperator.GetName: string;
begin
  Result := 'roll';
end;

{ TdxPDFPostScriptSubOperator }

procedure TdxPDFPostScriptSubOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
var
  ASubtrahend, AMinuend: TdxPDFBase;
begin
  ASubtrahend := AInterpreter.Stack.Pop as TdxPDFBase;
  AMinuend := AInterpreter.Stack.Pop as TdxPDFBase;
  if (AMinuend.ObjectType = otInteger) and (ASubtrahend.ObjectType = otInteger) then
    AInterpreter.Stack.Push(TdxPDFInteger.Create(TdxPDFInteger(AMinuend).Value - TdxPDFInteger(ASubtrahend).Value))
  else
    AInterpreter.Stack.Push(TdxPDFDouble.Create(TdxPDFUtils.ConvertToDouble(AMinuend) - TdxPDFUtils.ConvertToDouble(ASubtrahend)));
end;

class function TdxPDFPostScriptSubOperator.GetName: string;
begin
  Result := 'sub';
end;

{ TdxPDFPostScriptExchangeOperator }

procedure TdxPDFPostScriptExchangeOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
begin
  AInterpreter.Stack.Exchange;
end;

class function TdxPDFPostScriptExchangeOperator.GetName: string;
begin
  Result := 'exch';
end;

{ TdxPDFPostScriptPopOperator }

procedure TdxPDFPostScriptPopOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
begin
  AInterpreter.Stack.Pop;
end;

class function TdxPDFPostScriptPopOperator.GetName: string;
begin
  Result := 'pop';
end;

{ TdxPDFPostScriptDupOperator }

procedure TdxPDFPostScriptDupOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
begin
  AInterpreter.Stack.Push(AInterpreter.Stack.Peek);
end;

class function TdxPDFPostScriptDupOperator.GetName: string;
begin
  Result := 'dup';
end;

{ TdxPDFPostScriptMulOperator }

procedure TdxPDFPostScriptMulOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
var
  AOperand1, AOperand2: TObject;
begin
  AOperand1 := AInterpreter.Stack.Pop;
  AOperand2 := AInterpreter.Stack.Pop;
  if (AOperand1 is TdxPDFNumericObject) and (AOperand2 is TdxPDFNumericObject) then
    AInterpreter.Stack.Push(TdxPDFNumericObject.Create(TdxPDFNumericObjectAccess(AOperand1).GetValue *
      TdxPDFNumericObjectAccess(AOperand2).GetValue))
end;

class function TdxPDFPostScriptMulOperator.GetName: string;
begin
  Result := 'mul';
end;

{ TdxPDFPostScriptDictOperator }

procedure TdxPDFPostScriptDictOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
var
  ADictionary: TdxPDFPostScriptDictionary;
  AOperand: TdxPDFReferencedObject;
begin
  AOperand := AInterpreter.Stack.Pop;
  if AOperand <> nil then
  begin
    if AOperand is TdxPDFNumericObject then
    begin
      ADictionary := TdxPDFPostScriptDictionary.Create(TdxPDFNumericObjectAccess(AOperand).GetValue);
      AInterpreter.Stack.Push(ADictionary);
    end
    else
      TdxPDFUtils.RaiseTestException;
  end;
end;

class function TdxPDFPostScriptDictOperator.GetName: string;
begin
  Result := 'dict';
end;

{ TdxPDFPostScriptDefOperator }

procedure TdxPDFPostScriptDefOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
var
  AValue, AName: TObject;
  ADictionary: TdxPDFPostScriptDictionary;
begin
  if AInterpreter.DictionaryStack.Count > 0 then
  begin
    AValue := AInterpreter.Stack.Pop;
    AName := AInterpreter.Stack.Pop;
    if AName is TdxPDFName then
    begin
      ADictionary := AInterpreter.DictionaryStack.Peek as TdxPDFPostScriptDictionary;
      ADictionary.Add(AName as TdxPDFReferencedObject, AValue as TdxPDFReferencedObject);
    end;
  end;
end;

class function TdxPDFPostScriptDefOperator.GetName: string;
begin
  Result := 'def';
end;

{ TdxPDFPostScriptReadonlyOperator }

procedure TdxPDFPostScriptReadonlyOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
begin
  if AInterpreter.Stack.Count = 0 then
    TdxPDFUtils.RaiseTestException;
end;

class function TdxPDFPostScriptReadonlyOperator.GetName: string;
begin
  Result := 'readonly';
end;

{ TdxPDFPostScriptPutOperator }

procedure TdxPDFPostScriptPutOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
var
  AValue: TdxPDFReferencedObject;
  AKeyOrIndex, AContainer: TObject;
begin
  AValue := AInterpreter.Stack.Pop;
  AKeyOrIndex := AInterpreter.Stack.Pop;
  AContainer := AInterpreter.Stack.Pop;
  if AContainer is TdxPDFArray then
  begin
    if AKeyOrIndex is TdxPDFInteger then
    begin
      if TdxPDFInteger(AKeyOrIndex).Value < 0 then
        TdxPDFUtils.RaiseTestException;
      TdxPDFArray(AContainer).ElementList[TdxPDFInteger(AKeyOrIndex).Value] := AValue as TdxPDFBase;
    end
    else
      TdxPDFUtils.RaiseTestException;
  end
  else
    if (AContainer is TdxPDFPostScriptDictionary) and (AKeyOrIndex is TdxPDFString) then
      TdxPDFPostScriptDictionary(AContainer).Add(TdxPDFString(AKeyOrIndex), AValue)
    else
      TdxPDFUtils.RaiseTestException;
end;

class function TdxPDFPostScriptPutOperator.GetName: string;
begin
  Result := 'put';
end;

{ TdxPDFPostScriptForOperator }

procedure TdxPDFPostScriptForOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);

  procedure DoExecute(ALimit, AInitial, AIncrement: TdxPDFNumericObjectAccess; AProcedures: TdxPDFArray; AIsDouble: Boolean);
  var
    AValue: Double;
  begin
   AValue := AInitial.GetValue;
   if AIncrement.GetValue = 0 then
     TdxPDFUtils.RaiseTestException
   else
     if AIncrement.GetValue > 0 then
       while AValue <= ALimit.GetValue do
       begin
         if AIsDouble then
           AInterpreter.Stack.Push(TdxPDFDouble.Create(AValue))
         else
           AInterpreter.Stack.Push(TdxPDFInteger.Create(Trunc(AValue)));
         AInterpreter.Execute(AProcedures);
         AValue := AValue + AIncrement.GetValue;
       end
     else
       while AValue >= ALimit.GetValue do
       begin
         if AIsDouble then
           AInterpreter.Stack.Push(TdxPDFDouble.Create(AValue))
         else
           AInterpreter.Stack.Push(TdxPDFInteger.Create(Trunc(AValue)));
         AInterpreter.Execute(AProcedures);
         AValue := AValue + AIncrement.GetValue;
       end;
  end;

var
  ALimitValue, AIncrementValue, AInitialValue: TdxPDFNumericObject;
  AStack: TdxPDFPostScriptStack;
  AProcedures: TdxPDFArray;
  AIsDouble: Boolean;
begin
  AStack := AInterpreter.Stack;
  AProcedures := AStack.Pop as TdxPDFArray;
  if AProcedures <> nil then
  begin
    ALimitValue := AStack.Pop as TdxPDFNumericObject;
    AIncrementValue := AStack.Pop as TdxPDFNumericObject;
    AInitialValue := AStack.Pop as TdxPDFNumericObject;
    AIsDouble := (ALimitValue.ObjectType = otDouble) or (AInitialValue.ObjectType = otDouble) or
      (AIncrementValue.ObjectType = otDouble);
    DoExecute(TdxPDFNumericObjectAccess(ALimitValue), TdxPDFNumericObjectAccess(AInitialValue),
      TdxPDFNumericObjectAccess(AIncrementValue), AProcedures, AIsDouble);
  end;
end;

class function TdxPDFPostScriptForOperator.GetName: string;
begin
  Result := 'for';
end;

{ TdxPDFPostScriptEndOperator }

procedure TdxPDFPostScriptEndOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
begin
  if AInterpreter.DictionaryStack.Count > 0 then
    AInterpreter.DictionaryStack.Pop;
end;

class function TdxPDFPostScriptEndOperator.GetName: string;
begin
  Result:= 'end';
end;

{ TdxPDFPostScriptEexecOperator }

procedure TdxPDFPostScriptEexecOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
begin
// do nothing
end;

class function TdxPDFPostScriptEexecOperator.GetName: string;
begin
  Result := 'eexec';
end;

{ TdxPDFPostScriptReadStringOperator }

procedure TdxPDFPostScriptReadStringOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
var
  ACompleted: Boolean;
  AData: TBytes;
  AParser: TdxPDFPostScriptFileParser;
  AParserObject, AValue: TdxPDFReferencedObject;
begin
  AValue := AInterpreter.Stack.Pop;
  AParserObject := AInterpreter.Stack.Pop;
  if (AValue is TdxPDFString) and (AParserObject is TdxPDFPostScriptFileParser) then
  begin
    AParser := TdxPDFPostScriptFileParser(AParserObject);
    AData := TdxPDFUtils.StrToByteArray(TdxPDFString(AValue).Value);
    ACompleted := AParser.ReadString(AData) = Length(AData);
    AInterpreter.Stack.Push(TdxPDFString.Create(TdxPDFUtils.ConvertToStr(AData)));
    AInterpreter.Stack.Push(TdxPDFBoolean.Create(ACompleted));
  end
  else
    TdxPDFUtils.RaiseTestException;
end;

class function TdxPDFPostScriptReadStringOperator.GetName: string;
begin
   Result := 'readstring';
end;

{ TdxPDFPostScriptExecuteOnlyOperator }

procedure TdxPDFPostScriptExecuteOnlyOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
begin
  if not (AInterpreter.Stack.Peek is TdxPDFArray) then
    TdxPDFUtils.RaiseTestException;
end;

class function TdxPDFPostScriptExecuteOnlyOperator.GetName: string;
begin
  Result := 'executeonly';
end;

{ TdxPDFPostScriptNoAccessOperator }

procedure TdxPDFPostScriptNoAccessOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
var
  AObject: TdxPDFReferencedObject;
begin
  AObject := AInterpreter.Stack.Peek;
  if not (AObject is TdxPDFPostScriptDictionary) and not (AObject is TdxPDFString) and not (AObject is TdxPDFArray) then
    TdxPDFUtils.RaiseTestException;
end;

class function TdxPDFPostScriptNoAccessOperator.GetName: string;
begin
  Result := 'noaccess';
end;

{ TdxPDFPostScriptType1FontCustomOperator }

constructor TdxPDFPostScriptType1FontCustomOperator.Create;
begin
  inherited Create;
  InitializeTokens;
end;

procedure TdxPDFPostScriptType1FontCustomOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
var
  I: Integer;
begin
  for I := 0 to Length(FTokens) - 1 do
    if DoExecute(AInterpreter, FTokens[I]) then
      Break;
end;

function TdxPDFPostScriptType1FontCustomOperator.DoExecute(AInterpreter: TdxPDFPostScriptInterpreter;
  const AToken: string): Boolean;
var
  I: Integer;
  ADictionary: TdxPDFPostScriptDictionary;
begin
  for I := 0 to AInterpreter.DictionaryStack.List.Count - 1 do
  begin
    ADictionary := AInterpreter.DictionaryStack.List[I] as TdxPDFPostScriptDictionary;
    if DoExecute(AInterpreter, ADictionary, AToken) then
      Exit(True);
  end;
  Result := DoExecute(AInterpreter, AInterpreter.UserDictionary, AToken);
end;

function TdxPDFPostScriptType1FontCustomOperator.DoExecute(AInterpreter: TdxPDFPostScriptInterpreter;
  ADictionary: TdxPDFPostScriptDictionary; const AToken: string): Boolean;
var
  AObject: TdxPDFReferencedObject;
begin
  Result := ADictionary.TryGetValue(AToken, AObject);
  if Result then
    AInterpreter.Execute(AObject as TdxPDFArray);
end;

{ TdxPDFPostScriptType1FontRDOperator }

class function TdxPDFPostScriptType1FontRDOperator.GetName: string;
begin
  Result := 'RD';
end;

procedure TdxPDFPostScriptType1FontRDOperator.InitializeTokens;
begin
  SetLength(FTokens, 2);
  FTokens[0] := 'RD';
  FTokens[1] := '-|';
end;

{ TdxPDFPostScriptType1FontRDAlternativeOperator }

class function TdxPDFPostScriptType1FontRDAlternativeOperator.GetName: string;
begin
  Result := '-|';
end;

{ TdxPDFPostScriptType1FontNDOperator }

class function TdxPDFPostScriptType1FontNDOperator.GetName: string;
begin
  Result := 'ND';
end;

procedure TdxPDFPostScriptType1FontNDOperator.InitializeTokens;
begin
  SetLength(FTokens, 2);
  FTokens[0] := 'ND';
  FTokens[1] := '|-';
end;

{ TdxPDFPostScriptType1FontNDAlternativeOperator }

class function TdxPDFPostScriptType1FontNDAlternativeOperator.GetName: string;
begin
  Result := '|-'
end;

{ TdxPDFPostScriptType1FontNPOperator }

class function TdxPDFPostScriptType1FontNPOperator.GetName: string;
begin
  Result := 'NP';
end;

procedure TdxPDFPostScriptType1FontNPOperator.InitializeTokens;
begin
  SetLength(FTokens, 2);
  FTokens[0] := 'NP';
  FTokens[1] := '|';
end;

{ TdxPDFPostScriptType1FontNPAlternativeOperator }

class function TdxPDFPostScriptType1FontNPAlternativeOperator.GetName: string;
begin
  Result := '|';
end;

{ TdxPDFPostScriptGetOperator }

procedure TdxPDFPostScriptGetOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
var
  AKeyName: string;
  ATempObject, AObject: TdxPDFReferencedObject;
begin
  AObject := AInterpreter.Stack.Pop;
  if AObject is TdxPDFString then
  begin
    AKeyName := TdxPDFString(AObject).Value;
    AObject := AInterpreter.Stack.Pop;
    if not (AObject is TdxPDFPostScriptDictionary) then
    begin
      if AObject is TdxPDFType1FontClassicFontProgram then
        AInterpreter.Stack.Push(AObject)
      else
        TdxPDFUtils.RaiseTestException;
    end
    else
      if TdxPDFPostScriptDictionary(AObject).TryGetValue(AKeyName, ATempObject) then
        AInterpreter.Stack.Push(ATempObject as TdxPDFReferencedObject)
      else
        TdxPDFUtils.RaiseTestException;
  end
  else
    TdxPDFUtils.RaiseTestException;
end;

class function TdxPDFPostScriptGetOperator.GetName: string;
begin
  Result := 'get';
end;

{ TdxPDFPostScriptKnownOperator }

procedure TdxPDFPostScriptKnownOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
var
  AKeyObject: TdxPDFReferencedObject;
  ADictionaryObject: TObject;
begin
  AKeyObject := AInterpreter.Stack.Pop;
  ADictionaryObject := AInterpreter.Stack.Pop;
  if (AKeyObject is TdxPDFString) and (ADictionaryObject is TdxPDFPostScriptDictionary) then
    AInterpreter.Stack.Push(TdxPDFBoolean.Create(
      TdxPDFPostScriptDictionary(ADictionaryObject).TryGetValue(TdxPDFString(AKeyObject).Value,
      AKeyObject)))
  else
    TdxPDFUtils.RaiseTestException;
end;

class function TdxPDFPostScriptKnownOperator.GetName: string;
begin
  Result := 'known';
end;

{ TdxPDFPostScriptMarkOperator }

procedure TdxPDFPostScriptMarkOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
begin
  AInterpreter.Stack.Push(TdxPDFPostScriptMark.Create);
end;

class function TdxPDFPostScriptMarkOperator.GetName: string;
begin
  Result := 'mark';
end;

{ TdxPDFPostScriptFindFontOperator }

procedure TdxPDFPostScriptFindFontOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
var
  ANameObject, AProgramObject: TdxPDFReferencedObject;
begin
  ANameObject := AInterpreter.Stack.Pop;
  if (ANameObject is TdxPDFString) and  AInterpreter.FontDirectory.TryGetValue(TdxPDFString(ANameObject).Value, AProgramObject) and
    (AProgramObject is TdxPDFType1FontClassicFontProgram) then
      AInterpreter.Stack.Push(AProgramObject)
    else
      TdxPDFUtils.RaiseTestException;
end;

class function TdxPDFPostScriptFindFontOperator.GetName: string;
begin
  Result := 'findfont';
end;

{ TdxPDFPostScriptFontDirectoryOperator }

procedure TdxPDFPostScriptFontDirectoryOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
begin
  AInterpreter.Stack.Push(AInterpreter.FontDirectory);
end;

class function TdxPDFPostScriptFontDirectoryOperator.GetName: string;
begin
  Result := 'FontDirectory';
end;

{ TdxPDFPostScriptGeOperator }

procedure TdxPDFPostScriptGeOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
var
  AOperand1, AOperand2: Double;
begin
  AOperand2 := AInterpreter.Stack.PopAsDouble;
  AOperand1 := AInterpreter.Stack.PopAsDouble;
  AInterpreter.Stack.Push(TdxPDFBoolean.Create(AOperand1 >= AOperand2));
end;

class function TdxPDFPostScriptGeOperator.GetName: string;
begin
  Result := 'ge';
end;

{ TdxPDFPostScriptGtOperator }

procedure TdxPDFPostScriptGtOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
var
  AOperand1, AOperand2: Double;
begin
  AOperand2 := AInterpreter.Stack.PopAsDouble;
  AOperand1 := AInterpreter.Stack.PopAsDouble;
  AInterpreter.Stack.Push(TdxPDFBoolean.Create(AOperand1 > AOperand2));
end;

class function TdxPDFPostScriptGtOperator.GetName: string;
begin
  Result := 'gt';
end;

{ TdxPDFPostScriptIfElseOperator }

procedure TdxPDFPostScriptIfElseOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
var
  ACondition, AProc1, AProc2: TdxPDFReferencedObject;
begin
  AProc2 := AInterpreter.Stack.Pop;
  AProc1 := AInterpreter.Stack.Pop;
  ACondition := AInterpreter.Stack.Pop;
  if (ACondition is TdxPDFBoolean) and (AProc1 is TdxPDFArray) and (AProc1 is TdxPDFArray) then
  begin
    if TdxPDFBoolean(ACondition).Value then
      AInterpreter.Execute(TdxPDFArray(AProc1))
    else
      AInterpreter.Execute(TdxPDFArray(AProc2));
  end
  else
    TdxPDFUtils.RaiseTestException;
end;

class function TdxPDFPostScriptIfElseOperator.GetName: string;
begin
  Result := 'ifelse';
end;

{ TdxPDFPostScriptIfOperator }

procedure TdxPDFPostScriptIfOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
var
  ACondition, AProc: TdxPDFReferencedObject;
begin
  AProc := AInterpreter.Stack.Pop;
  ACondition := AInterpreter.Stack.Pop;
  if not ((ACondition is TdxPDFBoolean) and (AProc is TdxPDFArray)) then
    TdxPDFUtils.RaiseTestException;
  if TdxPDFBoolean(ACondition).Value  then
    AInterpreter.Execute(TdxPDFArray(AProc))
end;

class function TdxPDFPostScriptIfOperator.GetName: string;
begin
  Result := 'if';
end;

{ TdxPDFPostScriptLeOperator }

procedure TdxPDFPostScriptLeOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
var
  AOperand1, AOperand2: Double;
begin
  AOperand2 := AInterpreter.Stack.PopAsDouble;
  AOperand1 := AInterpreter.Stack.PopAsDouble;
  AInterpreter.Stack.Push(TdxPDFBoolean.Create(AOperand1 <= AOperand2));
end;

class function TdxPDFPostScriptLeOperator.GetName: string;
begin
  Result := 'le';
end;

{ TdxPDFPostScriptLtOperator }

procedure TdxPDFPostScriptLtOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
var
  AOperand1, AOperand2: Double;
begin
  AOperand2 := AInterpreter.Stack.PopAsDouble;
  AOperand1 := AInterpreter.Stack.PopAsDouble;
  AInterpreter.Stack.Push(TdxPDFBoolean.Create(AOperand1 < AOperand2));
end;

class function TdxPDFPostScriptLtOperator.GetName: string;
begin
  Result := 'lt';
end;

{ TdxPDFPostScriptStandardEncodingOperator }

procedure TdxPDFPostScriptStandardEncodingOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
var
  AArray: TdxPDFArray;
begin
  AArray := TdxPDFarray.Create;
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('space');
  AArray.Add('exclam');
  AArray.Add('quotedbl');
  AArray.Add('numbersign');
  AArray.Add('dollar');
  AArray.Add('percent');
  AArray.Add('ampersand');
  AArray.Add('quoteright');
  AArray.Add('parenleft');
  AArray.Add('parenright');
  AArray.Add('asterisk');
  AArray.Add('plus');
  AArray.Add('comma');
  AArray.Add('hyphen');
  AArray.Add('period');
  AArray.Add('slash');
  AArray.Add('zero');
  AArray.Add('one');
  AArray.Add('two');
  AArray.Add('three');
  AArray.Add('four');
  AArray.Add('five');
  AArray.Add('six');
  AArray.Add('seven');
  AArray.Add('eight');
  AArray.Add('nine');
  AArray.Add('colon');
  AArray.Add('semicolon');
  AArray.Add('less');
  AArray.Add('equal');
  AArray.Add('greater');
  AArray.Add('question');
  AArray.Add('at');
  AArray.Add('A');
  AArray.Add('B');
  AArray.Add('C');
  AArray.Add('D');
  AArray.Add('E');
  AArray.Add('F');
  AArray.Add('G');
  AArray.Add('H');
  AArray.Add('I');
  AArray.Add('J');
  AArray.Add('K');
  AArray.Add('L');
  AArray.Add('M');
  AArray.Add('N');
  AArray.Add('O');
  AArray.Add('P');
  AArray.Add('Q');
  AArray.Add('R');
  AArray.Add('S');
  AArray.Add('T');
  AArray.Add('U');
  AArray.Add('V');
  AArray.Add('W');
  AArray.Add('X');
  AArray.Add('Y');
  AArray.Add('Z');
  AArray.Add('bracketleft');
  AArray.Add('backslash');
  AArray.Add('bracketright');
  AArray.Add('asciicircum');
  AArray.Add('underscore');
  AArray.Add('quoteleft');
  AArray.Add('a');
  AArray.Add('b');
  AArray.Add('c');
  AArray.Add('d');
  AArray.Add('e');
  AArray.Add('f');
  AArray.Add('g');
  AArray.Add('h');
  AArray.Add('i');
  AArray.Add('j');
  AArray.Add('k');
  AArray.Add('l');
  AArray.Add('m');
  AArray.Add('n');
  AArray.Add('o');
  AArray.Add('p');
  AArray.Add('q');
  AArray.Add('r');
  AArray.Add('s');
  AArray.Add('t');
  AArray.Add('u');
  AArray.Add('v');
  AArray.Add('w');
  AArray.Add('x');
  AArray.Add('y');
  AArray.Add('z');
  AArray.Add('braceleft');
  AArray.Add('bar');
  AArray.Add('braceright');
  AArray.Add('asciitilde');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('.notdef');
  AArray.Add('dotlessi');
  AArray.Add('grave');
  AArray.Add('acute');
  AArray.Add('circumflex');
  AArray.Add('tilde');
  AArray.Add('macron');
  AArray.Add('breve');
  AArray.Add('dotaccent');
  AArray.Add('dieresis');
  AArray.Add('.notdef');
  AArray.Add('ring');
  AArray.Add('cedilla');
  AArray.Add('.notdef');
  AArray.Add('hungarumlaut');
  AArray.Add('ogonek');
  AArray.Add('caron');
  AArray.Add('space');
  AArray.Add('exclamdown');
  AArray.Add('cent');
  AArray.Add('sterling');
  AArray.Add('fraction');
  AArray.Add('yen');
  AArray.Add('florin');
  AArray.Add('section');
  AArray.Add('currency');
  AArray.Add('quotesingle');
  AArray.Add('quotedblleft');
  AArray.Add('guillemotleft');
  AArray.Add('guilsinglleft');
  AArray.Add('guilsinglright');
  AArray.Add('fi');
  AArray.Add('fl');
  AArray.Add('degree');
  AArray.Add('endash');
  AArray.Add('dagger');
  AArray.Add('daggerdbl');
  AArray.Add('periodcentered');
  AArray.Add('mu');
  AArray.Add('paragraph');
  AArray.Add('bullet');
  AArray.Add('quotesinglbase');
  AArray.Add('quotedblbase');
  AArray.Add('quotedblright');
  AArray.Add('guillemotright');
  AArray.Add('ellipsis');
  AArray.Add('perthousand');
  AArray.Add('threequarters');
  AArray.Add('questiondown');
  AArray.Add('Agrave');
  AArray.Add('grave');
  AArray.Add('acute');
  AArray.Add('circumflex');
  AArray.Add('tilde');
  AArray.Add('Aring');
  AArray.Add('breve');
  AArray.Add('dotaccent');
  AArray.Add('dieresis');
  AArray.Add('Eacute');
  AArray.Add('degree');
  AArray.Add('cedilla');
  AArray.Add('Igrave');
  AArray.Add('hungarumlaut');
  AArray.Add('ogonek');
  AArray.Add('caron');
  AArray.Add('emdash');
  AArray.Add('Ntilde');
  AArray.Add('Ograve');
  AArray.Add('Oacute');
  AArray.Add('Ocircumflex');
  AArray.Add('Otilde');
  AArray.Add('Odieresis');
  AArray.Add('multiply');
  AArray.Add('Oslash');
  AArray.Add('Ugrave');
  AArray.Add('Uacute');
  AArray.Add('Ucircumflex');
  AArray.Add('Udieresis');
  AArray.Add('Yacute');
  AArray.Add('Thorn');
  AArray.Add('germandbls');
  AArray.Add('agrave');
  AArray.Add('AE');
  AArray.Add('acircumflex');
  AArray.Add('ordfeminine');
  AArray.Add('adieresis');
  AArray.Add('aring');
  AArray.Add('ae');
  AArray.Add('ccedilla');
  AArray.Add('Lslash');
  AArray.Add('Oslash');
  AArray.Add('OE');
  AArray.Add('ordmasculine');
  AArray.Add('igrave');
  AArray.Add('iacute');
  AArray.Add('icircumflex');
  AArray.Add('idieresis');
  AArray.Add('eth');
  AArray.Add('ae');
  AArray.Add('ograve');
  AArray.Add('oacute');
  AArray.Add('ocircumflex');
  AArray.Add('onesuperior');
  AArray.Add('odieresis');
  AArray.Add('divide');
  AArray.Add('lslash');
  AArray.Add('oslash');
  AArray.Add('oe');
  AArray.Add('germandbls');
  AArray.Add('udieresis');
  AArray.Add('yacute');
  AArray.Add('thorn');
  AArray.Add('ydieresis');
  AInterpreter.Stack.Push(AArray);
end;

class function TdxPDFPostScriptStandardEncodingOperator.GetName: string;
begin
  Result := 'StandardEncoding';
end;

{ TdxPDFPostScriptSystemDictOperator }

procedure TdxPDFPostScriptSystemDictOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
begin
  AInterpreter.Stack.Push(AInterpreter.SystemDictionary);
end;

class function TdxPDFPostScriptSystemDictOperator.GetName: string;
begin
  Result := 'systemdict';
end;

{ TdxPDFPostScriptUserDictOperator }

procedure TdxPDFPostScriptUserDictOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
begin
  AInterpreter.Stack.Push(AInterpreter.UserDictionary);
end;

class function TdxPDFPostScriptUserDictOperator.GetName: string;
begin
  Result := 'userdict';
end;

{ TdxPDFPostScriptGetDictionaryElementOperator }

constructor TdxPDFPostScriptGetDictionaryElementOperator.Create(const AKey: string);
begin
  inherited Create;
   FKey := AKey;
end;

procedure TdxPDFPostScriptGetDictionaryElementOperator.Execute(AInterpreter: TdxPDFPostScriptInterpreter);
begin
// do nothing
end;

initialization
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptStringOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptAbsOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptAddOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptArrayOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptBeginOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptCvrOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptIndexOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptRollOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptSubOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptExchangeOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptPopOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptCloseFileOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptCurrentDictionaryOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptCurrentFileOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptDefineFontOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptDupOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptMulOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptDictOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptDefOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptReadonlyOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptPutOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptForOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptEndOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptEexecOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptReadStringOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptExecuteOnlyOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptNoAccessOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptType1FontRDOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptType1FontRDAlternativeOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptType1FontNDOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptType1FontNDAlternativeOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptType1FontNPOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptType1FontNPAlternativeOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptGetOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptKnownOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptMarkOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptFindFontOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptFontDirectoryOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptGeOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptGtOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptIfElseOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptIfOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptLeOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptLtOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptStandardEncodingOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptSystemDictOperator);
  dxPDFPostScriptOperatorFactory.RegisterOperator(TdxPDFPostScriptUserDictOperator);

finalization
  FreeAndNil(dxgPostScriptOperatorFactory);

end.


