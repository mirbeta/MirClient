{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressMapControl                                        }
{                                                                    }
{           Copyright (c) 2013-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSMAPCONTROL AND ALL             }
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

unit dxShapeFileInfo;

interface

{$I cxVer.inc}

uses
  Classes, SysUtils, RTLConsts, StrUtils, Windows, Math, Generics.Defaults, Generics.Collections,
  dxCore;

type
  TdxShapeFileRecordType = (sfrtNullShape = 0, sfrtPoint = 1, sfrtPolyLine = 3, sfrtPolygon = 5,
    sfrtMultiPoint = 8, sfrtPointZ = 11, sfrtPolyLineZ = 13, sfrtPolygonZ = 15, sfrtMultiPointZ = 18,
    sfrtPointM = 21, sfrtPolyLineM = 23, sfrtPolygonM = 25, sfrtMultiPointM = 28, sfrtMultiPatch = 31);

  TdxShapeFileHeader = record
    FileCode: Integer;
    Empty1: Integer;
    Empty2: Integer;
    Empty3: Integer;
    Empty4: Integer;
    Empty5: Integer;
    FileLength: Integer;
    Version: Integer;
    ShapeType: Integer;
    Xmin: Double;
    Ymin: Double;
    Xmax: Double;
    Ymax: Double;
    Zmin: Double;
    Zmax: Double;
    Mmin: Double;
    Mmax: Double;
  public
    procedure LoadFromStream(AStream: TStream);
  end;

  TdxShapeFilePoint = record
    X, Y: Double;
  public
    procedure LoadFromStream(AStream: TStream);
  end;

  TdxShapeFilePoints = array of TdxShapeFilePoint;

  TdxShapeFilePointM = record
    X, Y, M: Double;
  public
    procedure LoadFromStream(AStream: TStream);
    procedure LoadMFromStream(AStream: TStream);
    procedure LoadXYFromStream(AStream: TStream);
  end;

  TdxShapeFilePointsM = array of TdxShapeFilePointM;

  TdxShapeFilePointZ = record
    X, Y, Z, M: Double;
  public
    procedure LoadFromStream(AStream: TStream);
    procedure LoadMFromStream(AStream: TStream);
    procedure LoadXYFromStream(AStream: TStream);
    procedure LoadZFromStream(AStream: TStream);
  end;

  TdxShapeFilePointsZ = array of TdxShapeFilePointZ;

  TdxShapeFilePointRange = record
    Min, Max: Double;
  end;

  TdxShapeFileBoundingBox = record
    XRange: TdxShapeFilePointRange;
    YRange: TdxShapeFilePointRange;
  end;

  TdxShapeFileRecord = class
  private
    FRecordNumber: Integer;
  protected
    class function GetRecordType: TdxShapeFileRecordType; virtual;
  public
    constructor Create(ARecordNumber: Integer); virtual;
    procedure LoadFromStream(AStream: TStream); virtual;
    property RecordNumber: Integer read FRecordNumber;
  end;

  TdxShapeFileRecordClass = class of TdxShapeFileRecord;

  TdxShapeFilePointRecord = class(TdxShapeFileRecord)
  private
    FPoint: TdxShapeFilePoint;
  protected
    class function GetRecordType: TdxShapeFileRecordType; override;
  public
    procedure LoadFromStream(AStream: TStream); override;
    property Point: TdxShapeFilePoint read FPoint;
  end;

  TdxShapeFileBoundedShpRecord = class(TdxShapeFileRecord)
  private
    FBoundingBox: TdxShapeFileBoundingBox;
  public
    procedure LoadFromStream(AStream: TStream); override;
    property BoundingBox: TdxShapeFileBoundingBox read FBoundingBox;
  end;

  TdxShapeFileCustomMultiPointRecord = class(TdxShapeFileBoundedShpRecord)
  private
    FPoints: TdxShapeFilePoints;
    function GetPointCount: Integer;
    function GetPoints(Index: Integer): TdxShapeFilePoint;
  protected
    procedure ReadPointCount(AStream: TStream);
    procedure ReadPoints(AStream: TStream);
  public
    property PointCount: Integer read GetPointCount;
    property Points[Index: Integer]: TdxShapeFilePoint read GetPoints;
  end;

  TdxShapeFileMultiPointRecord = class(TdxShapeFileCustomMultiPointRecord)
  protected
    class function GetRecordType: TdxShapeFileRecordType; override;
  public
    procedure LoadFromStream(AStream: TStream); override;
  end;

  TdxShapeFilePolylineRecord = class(TdxShapeFileCustomMultiPointRecord)
  private
    FParts: array of Integer;
    function GetPartCount: Integer;
    function GetParts(Index: Integer): Integer;
  protected
    class function GetRecordType: TdxShapeFileRecordType; override;
  public
    procedure LoadFromStream(AStream: TStream); override;
    property PartCount: Integer read GetPartCount;
    property Parts[Index: Integer]: Integer read GetParts;
  end;

  TdxShapeFilePolygonRecord = class(TdxShapeFilePolylineRecord)
  protected
    class function GetRecordType: TdxShapeFileRecordType; override;
  end;

  TdxShapeFilePointMRecord = class(TdxShapeFileRecord)
  private
    FPointM: TdxShapeFilePointM;
  protected
    class function GetRecordType: TdxShapeFileRecordType; override;
  public
    procedure LoadFromStream(AStream: TStream); override;
    property PointM: TdxShapeFilePointM read FPointM;
  end;

  TdxShapeFileBoundedMShpRecord = class(TdxShapeFileBoundedShpRecord)
  private
    FMRange: TdxShapeFilePointRange;
  protected
    procedure ReadMRange(AStream: TStream);
  public
    property MRange: TdxShapeFilePointRange read FMRange;
  end;

  TdxShapeFileCustomMultiPointMRecord = class(TdxShapeFileBoundedMShpRecord)
  private
    FPointCount: Integer;
    FPointsM: TdxShapeFilePointsM;
    function GetPointsM(Index: Integer): TdxShapeFilePointM;
  protected
    procedure ReadMInfo(AStream: TStream);
    procedure ReadMPointsM(AStream: TStream);
    procedure ReadPointCount(AStream: TStream);
    procedure ReadPointsM(AStream: TStream);
  public
    property PointCount: Integer read FPointCount;
    property PointsM[Index: Integer]: TdxShapeFilePointM read GetPointsM;
  end;

  TdxShapeFileMultiPointMRecord = class(TdxShapeFileCustomMultiPointMRecord)
  protected
    class function GetRecordType: TdxShapeFileRecordType; override;
  public
    procedure LoadFromStream(AStream: TStream); override;
  end;

  TdxShapeFilePolylineMRecord = class(TdxShapeFileCustomMultiPointMRecord)
  private
    FParts: array of Integer;
    function GetPartCount: Integer;
    function GetParts(Index: Integer): Integer;
  protected
    class function GetRecordType: TdxShapeFileRecordType; override;
  public
    procedure LoadFromStream(AStream: TStream); override;
    property PartCount: Integer read GetPartCount;
    property Parts[Index: Integer]: Integer read GetParts;
  end;

  TdxShapeFilePolygonMRecord = class(TdxShapeFilePolylineMRecord)
  protected
    class function GetRecordType: TdxShapeFileRecordType; override;
  end;

  TdxShapeFilePointZRecord = class(TdxShapeFileRecord)
  private
    FPointZ: TdxShapeFilePointZ;
  protected
    class function GetRecordType: TdxShapeFileRecordType; override;
  public
    procedure LoadFromStream(AStream: TStream); override;
    property PointZ: TdxShapeFilePointZ read FPointZ;
  end;

  TdxShapeFileBoundedZShpRecord = class(TdxShapeFileBoundedMShpRecord)
  private
    FZRange: TdxShapeFilePointRange;
  protected
    procedure ReadZRange(AStream: TStream);
  public
    property ZRange: TdxShapeFilePointRange read FZRange;
  end;

  TdxShapeFileCustomMultiPointZRecord = class(TdxShapeFileBoundedZShpRecord)
  private
    FPointCount: Integer;
    FPointsZ: TdxShapeFilePointsZ;
    function GetPointsZ(Index: Integer): TdxShapeFilePointZ;
  protected
    procedure ReadMInfo(AStream: TStream);
    procedure ReadMPointsZ(AStream: TStream);
    procedure ReadPointCount(AStream: TStream);
    procedure ReadPointsZ(AStream: TStream);
    procedure ReadZInfo(AStream: TStream);
    procedure ReadZPointsZ(AStream: TStream);
  public
    property PointCount: Integer read FPointCount;
    property PointsZ[Index: Integer]: TdxShapeFilePointZ read GetPointsZ;
  end;

  TdxShapeFileMultiPointZRecord = class(TdxShapeFileCustomMultiPointZRecord)
  protected
    class function GetRecordType: TdxShapeFileRecordType; override;
  public
    procedure LoadFromStream(AStream: TStream); override;
  end;

  TdxShapeFilePolylineZRecord = class(TdxShapeFileCustomMultiPointZRecord)
  private
    FParts: array of Integer;
    function GetPartCount: Integer;
    function GetParts(Index: Integer): Integer;
  protected
    class function GetRecordType: TdxShapeFileRecordType; override;
    procedure ReadPartInfo(AStream: TStream); virtual;
  public
    procedure LoadFromStream(AStream: TStream); override;
    property PartCount: Integer read GetPartCount;
    property Parts[Index: Integer]: Integer read GetParts;
  end;

  TdxShapeFilePolygonZRecord = class(TdxShapeFilePolylineMRecord)
  protected
    class function GetRecordType: TdxShapeFileRecordType; override;
  end;

  TdxShapeFileMultiPatchRecordPartType = (sfptTriangleStrip, sfptTriangleFan, sfptOuterRing,
    sfptInnerRing, sfptFirstRing, sfptRing);

  TdxShapeFileMultiPatchRecord = class(TdxShapeFilePolylineZRecord)
  private
    FPartTypes: array of TdxShapeFileMultiPatchRecordPartType;
  protected
    class function GetRecordType: TdxShapeFileRecordType; override;
    procedure ReadPartInfo(AStream: TStream); override;
  end;

  TdxShapeFileReader = class
  public
    class function ReadBoundingBox(AStream: TStream): TdxShapeFileBoundingBox;
    class function ReadDouble(AStream: TStream): Double;
    class function ReadIntBE(AStream: TStream): Integer;
    class function ReadIntLE(AStream: TStream): Integer;
    class procedure ReadMPointsM(AStream: TStream; const APointsM: TdxShapeFilePointsM);
    class procedure ReadMPointsZ(AStream: TStream; const APointsZ: TdxShapeFilePointsZ);
    class procedure ReadParts(AStream: TStream; var AParts: array of Integer);
    class procedure ReadPartTypes(AStream: TStream; var APartTypes: array of TdxShapeFileMultiPatchRecordPartType);
    class procedure ReadPoint(AStream: TStream; var APoint: TdxShapeFilePoint);
    class procedure ReadPoints(AStream: TStream; const APoints: TdxShapeFilePoints);
    class procedure ReadPointsM(AStream: TStream; const APointsM: TdxShapeFilePointsM);
    class procedure ReadPointsZ(AStream: TStream; const APointsZ: TdxShapeFilePointsZ);
    class function ReadPointX(AStream: TStream): Double;
    class function ReadPointY(AStream: TStream): Double;
    class function ReadRange(AStream: TStream): TdxShapeFilePointRange;
    class procedure ReadZPointsZ(AStream: TStream; const APointsZ: TdxShapeFilePointsZ);
  end;

  TdxShapeFileInfo = class
  private
    FHeader: TdxShapeFileHeader;
    FRecords: TObjectList<TdxShapeFileRecord>;
    function CreateRecord(ARecordType: TdxShapeFileRecordType; ARecordNumber: Integer): TdxShapeFileRecord;
    procedure LoadHeader(AStream: TStream);
    procedure LoadRecord(AStream: TStream);
    procedure LoadRecords(AStream: TStream);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromStream(AStream: TStream);
    property Records: TObjectList<TdxShapeFileRecord> read FRecords;
  end;

implementation

type
  TdxShapeFileReaderClass = class of TdxShapeFileReader;

var
  Reader: TdxShapeFileReaderClass;

type
  TdxShapeFileRecordsFactory = class
  private
    FItems: TDictionary<TdxShapeFileRecordType, TdxShapeFileRecordClass>;
  protected
    function GetRecordClassByType(AType: TdxShapeFileRecordType): TdxShapeFileRecordClass;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateRecord(ARecordNumber: Integer; AType: TdxShapeFileRecordType): TdxShapeFileRecord;
    procedure RegisterRecord(AType: TdxShapeFileRecordType; AShapeFileRecordClass: TdxShapeFileRecordClass);
    procedure RegisterRecords(ARecordClasses: array of TdxShapeFileRecordClass);
  end;

var
  FShapeFileRecordsFactory: TdxShapeFileRecordsFactory;

function dxShapeFileRecordsFactory: TdxShapeFileRecordsFactory;
begin
  if FShapeFileRecordsFactory = nil then
    FShapeFileRecordsFactory := TdxShapeFileRecordsFactory.Create;
  Result := FShapeFileRecordsFactory;
end;

{ TdxShapeFileInfo }

constructor TdxShapeFileInfo.Create;
begin
  inherited Create;
  FRecords := TObjectList<TdxShapeFileRecord>.Create;
end;

function TdxShapeFileInfo.CreateRecord(
  ARecordType: TdxShapeFileRecordType; ARecordNumber: Integer): TdxShapeFileRecord;
begin
  Result := dxShapeFileRecordsFactory.CreateRecord(ARecordNumber, ARecordType);
end;

destructor TdxShapeFileInfo.Destroy;
begin
  FreeAndNil(FRecords);
  inherited;
end;

procedure TdxShapeFileInfo.LoadFromStream(AStream: TStream);
begin
  LoadHeader(AStream);
  LoadRecords(AStream);
end;

procedure TdxShapeFileInfo.LoadHeader(AStream: TStream);
begin
  FHeader.LoadFromStream(AStream);
end;

procedure TdxShapeFileInfo.LoadRecord(AStream: TStream);
var
  ARecordNumber, AContentLength: Integer;
  ARecordType: TdxShapeFileRecordType;
  ARecord: TdxShapeFileRecord;
begin
  ARecordNumber := Reader.ReadIntBE(AStream);
  AContentLength := Reader.ReadIntBE(AStream);
  ARecordType := TdxShapeFileRecordType(Reader.ReadIntLE(AStream));
  ARecord := CreateRecord(ARecordType, ARecordNumber);
  if ARecord <> nil then
  begin
    ARecord.LoadFromStream(AStream);
    FRecords.Add(ARecord);
  end
  else
    AStream.Seek(AContentLength * 2 - 4, soCurrent);
end;

procedure TdxShapeFileInfo.LoadRecords(AStream: TStream);
begin
  while (AStream.Position < AStream.Size) {and not(AStream.Position > 100000)} do
    LoadRecord(AStream);
end;

{ TdxShapeFileRecord }

constructor TdxShapeFileRecord.Create(ARecordNumber: Integer);
begin
  inherited Create;
  FRecordNumber := ARecordNumber;
end;

class function TdxShapeFileRecord.GetRecordType: TdxShapeFileRecordType;
begin
  Result := sfrtNullShape;
end;

procedure TdxShapeFileRecord.LoadFromStream(AStream: TStream);
begin
end;

{ TdxShapeFilePolylineRecord }

class function TdxShapeFilePolylineRecord.GetRecordType: TdxShapeFileRecordType;
begin
  Result := sfrtPolyLine;
end;

function TdxShapeFilePolylineRecord.GetPartCount: Integer;
begin
  Result := Length(FParts);
end;

function TdxShapeFilePolylineRecord.GetParts(Index: Integer): Integer;
begin
  Result := FParts[Index];
end;

procedure TdxShapeFilePolylineRecord.LoadFromStream(AStream: TStream);
var
  APartCount: Integer;
begin
  inherited LoadFromStream(AStream);
  APartCount := Reader.ReadIntLE(AStream);
  SetLength(FParts, APartCount);
  ReadPointCount(AStream);
  Reader.ReadParts(AStream, FParts);
  ReadPoints(AStream);
end;

{ TdxShapeFilePolygonRecord }

class function TdxShapeFilePolygonRecord.GetRecordType: TdxShapeFileRecordType;
begin
  Result := sfrtPolygon;
end;

{ TdxShapeFileBoundedShpRecord }

procedure TdxShapeFileBoundedShpRecord.LoadFromStream(AStream: TStream);
begin
  inherited LoadFromStream(AStream);
  FBoundingBox := Reader.ReadBoundingBox(AStream);
end;

{ TdxShapeFileReader }

class function TdxShapeFileReader.ReadBoundingBox(
  AStream: TStream): TdxShapeFileBoundingBox;
begin
  Result.XRange.Min := ReadDouble(AStream);
  Result.YRange.Min := ReadDouble(AStream);
  Result.XRange.Max := ReadDouble(AStream);
  Result.YRange.Max := ReadDouble(AStream);
end;

class function TdxShapeFileReader.ReadDouble(AStream: TStream): Double;
begin
  AStream.ReadBuffer(Result, SizeOf(Double));
end;

class function TdxShapeFileReader.ReadIntBE(AStream: TStream): Integer;
var
  ABytes: TBytes;
  I: Integer;
begin
  Result := 0;
  SetLength(ABytes, SizeOf(Result));
  AStream.ReadBuffer(ABytes[0], SizeOf(Result));
  for I := Low(ABytes) to High(ABytes) do
    Result := Result + ABytes[I] shl (8 * (High(ABytes) - I));
end;

class function TdxShapeFileReader.ReadIntLE(AStream: TStream): Integer;
var
  ABytes: TBytes;
  I: Integer;
begin
  Result := 0;
  SetLength(ABytes, SizeOf(Result));
  AStream.ReadBuffer(ABytes[0], SizeOf(Result));
  for I := Low(ABytes) to High(ABytes) do
    Result := Result + ABytes[I] shl (8 * I);
end;

class procedure TdxShapeFileReader.ReadMPointsM(AStream: TStream;
  const APointsM: TdxShapeFilePointsM);
var
  I: Integer;
begin
  for I := 0 to High(APointsM) do
    APointsM[I].LoadMFromStream(AStream);
end;

class procedure TdxShapeFileReader.ReadMPointsZ(AStream: TStream;
  const APointsZ: TdxShapeFilePointsZ);
var
  I: Integer;
begin
  for I := 0 to High(APointsZ) do
    APointsZ[I].LoadMFromStream(AStream);
end;

class procedure TdxShapeFileReader.ReadPoint(
  AStream: TStream; var APoint: TdxShapeFilePoint);
begin
  APoint.LoadFromStream(AStream);
end;

class procedure TdxShapeFileReader.ReadParts(AStream: TStream;
  var AParts: array of Integer);
var
  I: Integer;
begin
  for I := 0 to High(AParts) do
    AParts[I] := ReadIntLE(AStream);
end;

class procedure TdxShapeFileReader.ReadPartTypes(AStream: TStream;
  var APartTypes: array of TdxShapeFileMultiPatchRecordPartType);
var
  I: Integer;
begin
  for I := 0 to High(APartTypes) do
    APartTypes[I] := TdxShapeFileMultiPatchRecordPartType(ReadIntLE(AStream));
end;

class procedure TdxShapeFileReader.ReadPoints(AStream: TStream;
  const APoints: TdxShapeFilePoints);
var
  I: Integer;
begin
  for I := 0 to High(APoints) do
    ReadPoint(AStream, APoints[I]);
end;

class procedure TdxShapeFileReader.ReadPointsM(AStream: TStream;
  const APointsM: TdxShapeFilePointsM);
var
  I: Integer;
begin
  for I := 0 to High(APointsM) do
    APointsM[I].LoadXYFromStream(AStream);
end;

class procedure TdxShapeFileReader.ReadPointsZ(AStream: TStream;
  const APointsZ: TdxShapeFilePointsZ);
var
  I: Integer;
begin
  for I := 0 to High(APointsZ) do
    APointsZ[I].LoadXYFromStream(AStream);
end;

class function TdxShapeFileReader.ReadPointX(AStream: TStream): Double;
begin
  Result := Min(Max(ReadDouble(AStream), -180), 180);
end;

class function TdxShapeFileReader.ReadPointY(AStream: TStream): Double;
begin
  Result := Min(Max(ReadDouble(AStream), -90), 90);
end;

class function TdxShapeFileReader.ReadRange(
  AStream: TStream): TdxShapeFilePointRange;
begin
  Result.Min := ReadDouble(AStream);
  Result.Max := ReadDouble(AStream);
end;

class procedure TdxShapeFileReader.ReadZPointsZ(AStream: TStream;
  const APointsZ: TdxShapeFilePointsZ);
var
  I: Integer;
begin
  for I := 0 to High(APointsZ) do
    APointsZ[I].LoadZFromStream(AStream);
end;

{ TdxShapeFilePointRecord }

class function TdxShapeFilePointRecord.GetRecordType: TdxShapeFileRecordType;
begin
  Result := sfrtPoint;
end;

procedure TdxShapeFilePointRecord.LoadFromStream(AStream: TStream);
begin
  inherited LoadFromStream(AStream);
  FPoint.LoadFromStream(AStream);
end;

{ TdxShapeFileHeader }

procedure TdxShapeFileHeader.LoadFromStream(AStream: TStream);
begin
  FileCode := TdxShapeFileReader.ReadIntBE(AStream);
  Empty1 := TdxShapeFileReader.ReadIntBE(AStream);
  Empty2 := TdxShapeFileReader.ReadIntBE(AStream);
  Empty3 := TdxShapeFileReader.ReadIntBE(AStream);
  Empty4 := TdxShapeFileReader.ReadIntBE(AStream);
  Empty5 := TdxShapeFileReader.ReadIntBE(AStream);
  FileLength := TdxShapeFileReader.ReadIntBE(AStream);
  Version := TdxShapeFileReader.ReadIntLE(AStream);
  ShapeType := TdxShapeFileReader.ReadIntLE(AStream);
  Xmin := TdxShapeFileReader.ReadDouble(AStream);
  Ymin := TdxShapeFileReader.ReadDouble(AStream);
  Xmax := TdxShapeFileReader.ReadDouble(AStream);
  Ymax := TdxShapeFileReader.ReadDouble(AStream);
  Zmin := TdxShapeFileReader.ReadDouble(AStream);
  Zmax := TdxShapeFileReader.ReadDouble(AStream);
  Mmin := TdxShapeFileReader.ReadDouble(AStream);
  Mmax := TdxShapeFileReader.ReadDouble(AStream);
end;

{ TdxShapeFilePoint }

procedure TdxShapeFilePoint.LoadFromStream(AStream: TStream);
begin
  X := Reader.ReadPointX(AStream);
  Y := Reader.ReadPointY(AStream);
end;

{ TdxShapeFilePointM }

procedure TdxShapeFilePointM.LoadFromStream(AStream: TStream);
begin
  LoadXYFromStream(AStream);
  LoadMFromStream(AStream);
end;

procedure TdxShapeFilePointM.LoadMFromStream(AStream: TStream);
begin
  M := Reader.ReadDouble(AStream);
end;

procedure TdxShapeFilePointM.LoadXYFromStream(AStream: TStream);
begin
  X := Reader.ReadPointX(AStream);
  Y := Reader.ReadPointY(AStream);
end;

{ TdxShapeFilePointZ }

procedure TdxShapeFilePointZ.LoadFromStream(AStream: TStream);
begin
  LoadXYFromStream(AStream);
  LoadZFromStream(AStream);
  LoadMFromStream(AStream);
end;

procedure TdxShapeFilePointZ.LoadMFromStream(AStream: TStream);
begin
  M := Reader.ReadDouble(AStream);
end;

procedure TdxShapeFilePointZ.LoadXYFromStream(AStream: TStream);
begin
  X := Reader.ReadPointX(AStream);
  Y := Reader.ReadPointY(AStream);
end;

procedure TdxShapeFilePointZ.LoadZFromStream(AStream: TStream);
begin
  Z := Reader.ReadDouble(AStream);
end;

{ TdxShapeFileMultiPointRecord }

class function TdxShapeFileMultiPointRecord.GetRecordType: TdxShapeFileRecordType;
begin
  Result := sfrtMultiPoint;
end;

procedure TdxShapeFileMultiPointRecord.LoadFromStream(AStream: TStream);
begin
  inherited LoadFromStream(AStream);
  ReadPointCount(AStream);
  ReadPoints(AStream);
end;

{ TdxShapeFilePointMRecord }

class function TdxShapeFilePointMRecord.GetRecordType: TdxShapeFileRecordType;
begin
  Result := sfrtPointM;
end;

procedure TdxShapeFilePointMRecord.LoadFromStream(AStream: TStream);
begin
  FPointM.LoadFromStream(AStream);
end;

{ TdxShapeFileMultiPointMRecord }

class function TdxShapeFileMultiPointMRecord.GetRecordType: TdxShapeFileRecordType;
begin
  Result := sfrtMultiPointM;
end;

procedure TdxShapeFileMultiPointMRecord.LoadFromStream(AStream: TStream);
begin
  inherited LoadFromStream(AStream);
  ReadPointCount(AStream);
  ReadPointsM(AStream);
  ReadMInfo(AStream);
end;

{ TdxShapeFileBoundedMShpRecord }

function TdxShapeFileCustomMultiPointMRecord.GetPointsM(
  Index: Integer): TdxShapeFilePointM;
begin
  Result := FPointsM[Index];
end;

procedure TdxShapeFileCustomMultiPointMRecord.ReadMInfo(AStream: TStream);
begin
  ReadMRange(AStream);
  ReadMPointsM(AStream);
end;

procedure TdxShapeFileCustomMultiPointMRecord.ReadMPointsM(AStream: TStream);
begin
  Reader.ReadMPointsM(AStream, FPointsM);
end;

procedure TdxShapeFileCustomMultiPointMRecord.ReadPointCount(AStream: TStream);
var
  APointCount: Integer;
begin
  APointCount := Reader.ReadIntLE(AStream);
  SetLength(FPointsM, APointCount);
end;

procedure TdxShapeFileCustomMultiPointMRecord.ReadPointsM(AStream: TStream);
begin
  Reader.ReadPointsM(AStream, FPointsM);
end;

{ TdxShapeFileBoundedMShpRecord }

procedure TdxShapeFileBoundedMShpRecord.ReadMRange(AStream: TStream);
begin
  FMRange := Reader.ReadRange(AStream);
end;

{ TdxShapeFileRecordsFactory }

constructor TdxShapeFileRecordsFactory.Create;
begin
  inherited Create;
  FItems := TDictionary<TdxShapeFileRecordType, TdxShapeFileRecordClass>.Create;
end;

function TdxShapeFileRecordsFactory.CreateRecord(ARecordNumber: Integer;
  AType: TdxShapeFileRecordType): TdxShapeFileRecord;
var
  ARecordClass: TdxShapeFileRecordClass;
begin
  ARecordClass := GetRecordClassByType(AType);
  if ARecordClass <> nil then
    Result := ARecordClass.Create(ARecordNumber)
  else
    Result := nil;
end;

destructor TdxShapeFileRecordsFactory.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TdxShapeFileRecordsFactory.GetRecordClassByType(
  AType: TdxShapeFileRecordType): TdxShapeFileRecordClass;
begin
  if not FItems.TryGetValue(AType, Result) then
    Result := nil;
end;

procedure TdxShapeFileRecordsFactory.RegisterRecord(
  AType: TdxShapeFileRecordType;
  AShapeFileRecordClass: TdxShapeFileRecordClass);
begin
  FItems.Add(AType, AShapeFileRecordClass);
end;

procedure TdxShapeFileRecordsFactory.RegisterRecords(
  ARecordClasses: array of TdxShapeFileRecordClass);
var
  I: Integer;
begin
  for I := Low(ARecordClasses) to High(ARecordClasses) do
    RegisterRecord(ARecordClasses[I].GetRecordType, ARecordClasses[I]);
end;

{ TdxShapeFilePolylineMRecord }

function TdxShapeFilePolylineMRecord.GetPartCount: Integer;
begin
  Result := Length(FParts);
end;

function TdxShapeFilePolylineMRecord.GetParts(Index: Integer): Integer;
begin
  Result := FParts[Index];
end;

class function TdxShapeFilePolylineMRecord.GetRecordType: TdxShapeFileRecordType;
begin
  Result := sfrtPolyLineM;
end;

procedure TdxShapeFilePolylineMRecord.LoadFromStream(AStream: TStream);
var
  APartCount: Integer;
begin
  inherited LoadFromStream(AStream);
  APartCount := Reader.ReadIntLE(AStream);
  SetLength(FParts, APartCount);
  ReadPointCount(AStream);
  Reader.ReadParts(AStream, FParts);
  ReadPointsM(AStream);
  ReadMInfo(AStream);
end;

{ TdxShapeFilePolygonMRecord }

class function TdxShapeFilePolygonMRecord.GetRecordType: TdxShapeFileRecordType;
begin
  Result := sfrtPolygonM;
end;

{ TdxShapeFileCustomMultiPointRecord }

function TdxShapeFileCustomMultiPointRecord.GetPointCount: Integer;
begin
  Result := Length(FPoints);
end;

function TdxShapeFileCustomMultiPointRecord.GetPoints(
  Index: Integer): TdxShapeFilePoint;
begin
  Result := FPoints[Index];
end;

procedure TdxShapeFileCustomMultiPointRecord.ReadPointCount(AStream: TStream);
var
  APointCount: Integer;
begin
  APointCount := Reader.ReadIntLE(AStream);
  SetLength(FPoints, APointCount);
end;

procedure TdxShapeFileCustomMultiPointRecord.ReadPoints(AStream: TStream);
begin
  Reader.ReadPoints(AStream, FPoints);
end;

{ TdxShapeFilePointZRecord }

class function TdxShapeFilePointZRecord.GetRecordType: TdxShapeFileRecordType;
begin
  Result := sfrtPointZ;
end;

procedure TdxShapeFilePointZRecord.LoadFromStream(AStream: TStream);
begin
  FPointZ.LoadFromStream(AStream);
end;

{ TdxShapeFileBoundedZShpRecord }

procedure TdxShapeFileBoundedZShpRecord.ReadZRange(AStream: TStream);
begin
  FZRange := Reader.ReadRange(AStream);
end;

{ TdxShapeFileMultiPointZRecord }

class function TdxShapeFileMultiPointZRecord.GetRecordType: TdxShapeFileRecordType;
begin
  Result := sfrtMultiPointZ;
end;

procedure TdxShapeFileMultiPointZRecord.LoadFromStream(AStream: TStream);
begin
  inherited LoadFromStream(AStream);
  ReadPointCount(AStream);
  ReadPointsZ(AStream);
  ReadZInfo(AStream);
  ReadMInfo(AStream);
end;

{ TdxShapeFilePolylineZRecord }

function TdxShapeFilePolylineZRecord.GetPartCount: Integer;
begin
  Result := Length(FParts);
end;

function TdxShapeFilePolylineZRecord.GetParts(Index: Integer): Integer;
begin
  Result := FParts[Index];
end;

class function TdxShapeFilePolylineZRecord.GetRecordType: TdxShapeFileRecordType;
begin
  Result := sfrtPolyLineZ;
end;

procedure TdxShapeFilePolylineZRecord.LoadFromStream(AStream: TStream);
var
  APartCount: Integer;
begin
  inherited LoadFromStream(AStream);
  APartCount := Reader.ReadIntLE(AStream);
  SetLength(FParts, APartCount);
  ReadPointCount(AStream);
  Reader.ReadParts(AStream, FParts);
  ReadPartInfo(AStream);
  ReadPointCount(AStream);
  ReadPointsZ(AStream);
  ReadZInfo(AStream);
  ReadMInfo(AStream);
end;

procedure TdxShapeFilePolylineZRecord.ReadPartInfo(AStream: TStream);
begin
end;

{ TdxShapeFilePolygonZRecord }

class function TdxShapeFilePolygonZRecord.GetRecordType: TdxShapeFileRecordType;
begin
  Result := sfrtPolygonZ;
end;

{ TdxShapeFileCustomMultiPointZRecord }

function TdxShapeFileCustomMultiPointZRecord.GetPointsZ(
  Index: Integer): TdxShapeFilePointZ;
begin
  Result := FPointsZ[Index];
end;

procedure TdxShapeFileCustomMultiPointZRecord.ReadMInfo(AStream: TStream);
begin
  ReadMRange(AStream);
  ReadMPointsZ(AStream);
end;

procedure TdxShapeFileCustomMultiPointZRecord.ReadMPointsZ(AStream: TStream);
begin
  Reader.ReadMPointsZ(AStream, FPointsZ);
end;

procedure TdxShapeFileCustomMultiPointZRecord.ReadPointCount(AStream: TStream);
var
  APointCount: Integer;
begin
  APointCount := Reader.ReadIntLE(AStream);
  SetLength(FPointsZ, APointCount);
end;

procedure TdxShapeFileCustomMultiPointZRecord.ReadPointsZ(AStream: TStream);
begin
  Reader.ReadPointsZ(AStream, FPointsZ);
end;

procedure TdxShapeFileCustomMultiPointZRecord.ReadZInfo(AStream: TStream);
begin
  ReadZRange(AStream);
  ReadZPointsZ(AStream);
end;

procedure TdxShapeFileCustomMultiPointZRecord.ReadZPointsZ(AStream: TStream);
begin
  Reader.ReadZPointsZ(AStream, FPointsZ);
end;

{ TdxShapeFileMultiPatchRecord }

class function TdxShapeFileMultiPatchRecord.GetRecordType: TdxShapeFileRecordType;
begin
  Result := sfrtMultiPatch;
end;

procedure TdxShapeFileMultiPatchRecord.ReadPartInfo(AStream: TStream);
begin
  SetLength(FPartTypes, PartCount);
  Reader.ReadPartTypes(AStream, FPartTypes);
end;

initialization
  dxShapeFileRecordsFactory.RegisterRecords([TdxShapeFileRecord, TdxShapeFilePointRecord, TdxShapeFileMultiPointRecord,
    TdxShapeFilePolylineRecord, TdxShapeFilePolygonRecord, TdxShapeFilePointMRecord, TdxShapeFileMultiPointMRecord,
    TdxShapeFilePolylineMRecord, TdxShapeFilePolygonMRecord, TdxShapeFilePointZRecord, TdxShapeFileMultiPointZRecord,
    TdxShapeFilePolylineZRecord, TdxShapeFilePolygonZRecord, TdxShapeFileMultiPatchRecord]);

finalization
  FreeAndNil(FShapeFileRecordsFactory);

end.
