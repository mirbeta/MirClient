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

unit dxMapControlShapeFileLoader;

interface

{$I cxVer.inc}

uses
  Types, Classes, Graphics, SysUtils, Math, RTLConsts, Generics.Defaults, Generics.Collections,
  cxGraphics, cxGeometry, dxCore, dxCoreClasses, dxGDIPlusClasses, dxCoreGraphics,
  dxMapItem, dxMapControlTypes, dxMapItemFileLayer, dxShapeFileInfo, dxDbfFileInfo;

type
  TdxMapControlShapeFileLoader = class;

  TdxMapControlShapeFileRecordConverter = class
  private
    FLoader: TdxMapControlShapeFileLoader;
  protected
    procedure CreateAttributes(AMapItem: TdxMapItem; ADbfFileRecord: TdxDbfFileRecord);
    class function GetRecordClass: TdxShapeFileRecordClass; virtual;
  public
    constructor Create(ALoader: TdxMapControlShapeFileLoader); virtual;
    procedure Convert(AShapeFileRecord: TdxShapeFileRecord; ADbfFileRecord: TdxDbfFileRecord); virtual; abstract;
  end;

  TdxMapControlShapeFileRecordConverterClass = class of TdxMapControlShapeFileRecordConverter;

  TdxMapControlShapeFilePointRecordConverter = class(TdxMapControlShapeFileRecordConverter)
  protected
    class function GetRecordClass: TdxShapeFileRecordClass; override;
    function GetPointLocation(ARecord: TdxShapeFileRecord): TdxMapControlGeoPoint; virtual;
  public
    procedure Convert(AShapeFileRecord: TdxShapeFileRecord; ADbfFileRecord: TdxDbfFileRecord); override;
  end;

  TdxMapControlShapeFileMultiPointRecordConverter = class(TdxMapControlShapeFileRecordConverter)
  protected
    function GetPointCount(ARecord: TdxShapeFileRecord): Integer; virtual;
    function GetPointLocation(ARecord: TdxShapeFileRecord; APointIndex: Integer): TdxMapControlGeoPoint; virtual;
    class function GetRecordClass: TdxShapeFileRecordClass; override;
  public
    procedure Convert(AShapeFileRecord: TdxShapeFileRecord; ADbfFileRecord: TdxDbfFileRecord); override;
  end;

  TdxMapControlShapeFilePolylineRecordConverter = class(TdxMapControlShapeFileRecordConverter)
  protected
    function GetPointCount(ARecord: TdxShapeFileRecord): Integer; virtual;
    function GetPointLocation(ARecord: TdxShapeFileRecord; APointIndex: Integer): TdxMapControlGeoPoint; virtual;
    function GetPartCount(ARecord: TdxShapeFileRecord): Integer; virtual;
    function GetPart(ARecord: TdxShapeFileRecord; AIndex: Integer): Integer; virtual;
    class function GetRecordClass: TdxShapeFileRecordClass; override;
    function GetSegmentType: TdxMapSegmentType; virtual;
  public
    procedure Convert(AShapeFileRecord: TdxShapeFileRecord; ADbfFileRecord: TdxDbfFileRecord); override;
  end;

  TdxMapControlShapeFilePolygonRecordConverter = class(TdxMapControlShapeFilePolylineRecordConverter)
  protected
    class function GetRecordClass: TdxShapeFileRecordClass; override;
    function GetSegmentType: TdxMapSegmentType; override;
  end;

  TdxMapControlShapeFilePointMRecordConverter = class(TdxMapControlShapeFilePointRecordConverter)
  protected
    function GetPointLocation(ARecord: TdxShapeFileRecord): TdxMapControlGeoPoint; override;
    class function GetRecordClass: TdxShapeFileRecordClass; override;
  end;

  TdxMapControlShapeFileMultiPointMRecordConverter = class(TdxMapControlShapeFileMultiPointRecordConverter)
  protected
    function GetPointCount(ARecord: TdxShapeFileRecord): Integer; override;
    function GetPointLocation(ARecord: TdxShapeFileRecord; APointIndex: Integer): TdxMapControlGeoPoint; override;
    class function GetRecordClass: TdxShapeFileRecordClass; override;
  end;

  TdxMapControlShapeFilePolylineMRecordConverter = class(TdxMapControlShapeFilePolylineRecordConverter)
  protected
    function GetPointCount(ARecord: TdxShapeFileRecord): Integer; override;
    function GetPointLocation(ARecord: TdxShapeFileRecord; APointIndex: Integer): TdxMapControlGeoPoint; override;
    function GetPartCount(ARecord: TdxShapeFileRecord): Integer; override;
    function GetPart(ARecord: TdxShapeFileRecord; AIndex: Integer): Integer; override;
    class function GetRecordClass: TdxShapeFileRecordClass; override;
  end;

  TdxMapControlShapeFilePolygonMRecordConverter = class(TdxMapControlShapeFilePolygonRecordConverter)
  protected
    function GetPointCount(ARecord: TdxShapeFileRecord): Integer; override;
    function GetPointLocation(ARecord: TdxShapeFileRecord; APointIndex: Integer): TdxMapControlGeoPoint; override;
    function GetPartCount(ARecord: TdxShapeFileRecord): Integer; override;
    function GetPart(ARecord: TdxShapeFileRecord; AIndex: Integer): Integer; override;
    class function GetRecordClass: TdxShapeFileRecordClass; override;
  end;

  TdxMapControlShapeFileLoader = class
  private
    FMapItems: TdxMapItems;
    FOwner: TdxMapItemFileLayer;
  protected
    property MapItems: TdxMapItems read FMapItems;
  public
    constructor Create(AOwner: TdxMapItemFileLayer);
    procedure LoadFromStream(AShpStream, ADbfStream: TStream);
  end;

implementation

type
  TdxMapControlShapeFileRecordsConverterFactory = class
  private
    FItems: TdxFastList;
    FConvertersCache: TObjectDictionary<TdxShapeFileRecordClass, TdxMapControlShapeFileRecordConverter>;
    procedure RegisterConverter(AConverterClass: TdxMapControlShapeFileRecordConverterClass);
  public
    constructor Create;
    destructor Destroy; override;
    function GetConverter(ARecord: TdxShapeFileRecord): TdxMapControlShapeFileRecordConverter;
    procedure InitializeConverters(ALoader: TdxMapControlShapeFileLoader);
    procedure FinalizeConverters;
    procedure RegisterConverters(AConverterClasses: array of TdxMapControlShapeFileRecordConverterClass);
  end;

{ TdxMapControlShapeFileRecordsConverterFactory }

constructor TdxMapControlShapeFileRecordsConverterFactory.Create;
begin
  inherited Create;
  FItems := TdxFastList.Create;
end;

destructor TdxMapControlShapeFileRecordsConverterFactory.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TdxMapControlShapeFileRecordsConverterFactory.FinalizeConverters;
begin
  FreeAndNil(FConvertersCache);
end;

function TdxMapControlShapeFileRecordsConverterFactory.GetConverter(
  ARecord: TdxShapeFileRecord): TdxMapControlShapeFileRecordConverter;
begin
  if not FConvertersCache.TryGetValue(TdxShapeFileRecordClass(ARecord.ClassType), Result) then
    Result := nil;
end;

procedure TdxMapControlShapeFileRecordsConverterFactory.InitializeConverters(
  ALoader: TdxMapControlShapeFileLoader);
var
  I: Integer;
  AConverterClass: TdxMapControlShapeFileRecordConverterClass;
begin
  FConvertersCache := TObjectDictionary<TdxShapeFileRecordClass, TdxMapControlShapeFileRecordConverter>.Create([doOwnsValues]);
  for I := 0 to FItems.Count - 1 do
  begin
    AConverterClass := TdxMapControlShapeFileRecordConverterClass(FItems[I]);
    FConvertersCache.Add(AConverterClass.GetRecordClass, AConverterClass.Create(ALoader));
  end;
end;

procedure TdxMapControlShapeFileRecordsConverterFactory.RegisterConverter(
  AConverterClass: TdxMapControlShapeFileRecordConverterClass);
begin
  FItems.Add(AConverterClass);
end;

procedure TdxMapControlShapeFileRecordsConverterFactory.RegisterConverters(
  AConverterClasses: array of TdxMapControlShapeFileRecordConverterClass);
var
  I: Integer;
begin
  for I := Low(AConverterClasses) to High(AConverterClasses) do
    RegisterConverter(AConverterClasses[I]);
end;

var
  FShapeFileRecordsConverterFactory: TdxMapControlShapeFileRecordsConverterFactory;

function dxMapControlShapeFileRecordsConverters: TdxMapControlShapeFileRecordsConverterFactory;
begin
  if FShapeFileRecordsConverterFactory = nil then
    FShapeFileRecordsConverterFactory := TdxMapControlShapeFileRecordsConverterFactory.Create;
  Result := FShapeFileRecordsConverterFactory;
end;

{ TdxMapControlShapeFileLoader }

constructor TdxMapControlShapeFileLoader.Create(AOwner: TdxMapItemFileLayer);
begin
  inherited Create;
  FOwner := AOwner;
  FMapItems := FOwner.MapItems;
end;

procedure TdxMapControlShapeFileLoader.LoadFromStream(AShpStream,
  ADbfStream: TStream);
var
  AShapeFileInfo: TdxShapeFileInfo;
  ADbfFileInfo: TdxDbfFileInfo;
  AConverter: TdxMapControlShapeFileRecordConverter;
  AShapeFileRecord: TdxShapeFileRecord;
  ADbfFileRecord: TdxDbfFileRecord;
begin
  AShapeFileInfo := TdxShapeFileInfo.Create;
  ADbfFileInfo := TdxDbfFileInfo.Create;
  try
    AShapeFileInfo.LoadFromStream(AShpStream);
    ADbfFileInfo.LoadFromStream(ADbfStream);
    MapItems.BeginUpdate;
    dxMapControlShapeFileRecordsConverters.InitializeConverters(Self);
    try
      for AShapeFileRecord in AShapeFileInfo.Records do
      begin
        AConverter := dxMapControlShapeFileRecordsConverters.GetConverter(AShapeFileRecord);
        if AConverter <> nil then
        begin
          ADbfFileRecord := ADbfFileInfo.Records[AShapeFileRecord.RecordNumber - 1];
          AConverter.Convert(AShapeFileRecord, ADbfFileRecord);
        end;
      end;
    finally
      dxMapControlShapeFileRecordsConverters.FinalizeConverters;
      MapItems.EndUpdate;
    end;
  finally
    ADbfFileInfo.Free;
    AShapeFileInfo.Free;
  end;
end;

{ TdxMapControlShapeFilePointRecordConverter }

procedure TdxMapControlShapeFilePointRecordConverter.Convert(
  AShapeFileRecord: TdxShapeFileRecord; ADbfFileRecord: TdxDbfFileRecord);
var
  AMapDot: TdxMapDot;
begin
  AMapDot := FLoader.MapItems.Add(TdxMapDot) as TdxMapDot;
  AMapDot.Location.GeoPoint := GetPointLocation(AShapeFileRecord);
  CreateAttributes(AMapDot, ADbfFileRecord);
end;

function TdxMapControlShapeFilePointRecordConverter.GetPointLocation(
  ARecord: TdxShapeFileRecord): TdxMapControlGeoPoint;
var
  APointRecord: TdxShapeFilePointRecord;
begin
  APointRecord := ARecord as TdxShapeFilePointRecord;
  Result := dxMapControlGeoPoint(APointRecord.Point.Y, APointRecord.Point.X);
end;

class function TdxMapControlShapeFilePointRecordConverter.GetRecordClass: TdxShapeFileRecordClass;
begin
  Result := TdxShapeFilePointRecord;
end;

{ TdxMapControlShapeFileRecordConverter }

constructor TdxMapControlShapeFileRecordConverter.Create(
  ALoader: TdxMapControlShapeFileLoader);
begin
  inherited Create;
  FLoader := ALoader;
end;

procedure TdxMapControlShapeFileRecordConverter.CreateAttributes(
  AMapItem: TdxMapItem; ADbfFileRecord: TdxDbfFileRecord);
var
  AField: TdxDbfFileField;
  AValue: string;
  AValueDouble: Double;
  AValueInt: Integer;
  AValueInt64: Int64;
  AValueDateTime: TDateTime;
  AValueBoolean: Boolean;
  AYear, AMonth, ADay: Word;
begin
  for AField in ADbfFileRecord.Fields.Keys do
  begin
    AValue := ADbfFileRecord.Fields.Items[AField];
    AMapItem.Attributes.Add(AField.FieldName, Variant(AValue));
    case Char(AField.FieldType) of
      'B', 'F':
         begin
           AValueDouble := dxStrToFloat(AValue);
           AMapItem.Attributes.Add(AField.FieldName, AValueDouble);
         end;
      'I':
        begin
          AValueInt := StrToInt(AValue);
          AMapItem.Attributes.Add(AField.FieldName, AValueInt);
        end;
      'N':
        begin
          if AField.NumberOfDecimalPlaces > 0 then
          begin
            AValueDouble := dxStrToFloat(AValue);
            AMapItem.Attributes.Add(AField.FieldName, AValueDouble);
          end
          else
            if AField.FieldLengthInBytes > 9 then
            begin
              AValueInt64 := StrToInt(AValue);
              AMapItem.Attributes.Add(AField.FieldName, AValueInt64);
            end
            else
            begin
              AValueInt := StrToInt(AValue);
              AMapItem.Attributes.Add(AField.FieldName, AValueInt);
            end;
        end;
      'D':
        begin
          if SameText(AValue, '') then
            AValueDateTime := MinDateTime
          else
          begin
            AYear := StrToInt(Copy(AValue, 1, 4));
            AMonth := StrToInt(Copy(AValue, 5, 2));
            ADay := StrToInt(Copy(AValue, 7, 2));
            AValueDateTime := EncodeDate(AYear, AMonth, ADay);
          end;
          AMapItem.Attributes.Add(AField.FieldName, AValueDateTime);
        end;
      'L':
        begin
          AValueBoolean := StrToBool(AValue);
          AMapItem.Attributes.Add(AField.FieldName, AValueBoolean);
        end
      else
        AMapItem.Attributes.Add(AField.FieldName, AValue);
    end;
  end;
end;

class function TdxMapControlShapeFileRecordConverter.GetRecordClass: TdxShapeFileRecordClass;
begin
  Result := nil;
end;

{ TdxMapControlShapeFilePolylineRecordConverter }

procedure TdxMapControlShapeFilePolylineRecordConverter.Convert(
  AShapeFileRecord: TdxShapeFileRecord; ADbfFileRecord: TdxDbfFileRecord);
var
  APartCount, APointCount: Integer;
  AMapPath: TdxMapPath;
  AMapPathSegment: TdxMapPathSegment;
  I, J, AEndPointIndex: Integer;
begin
  AMapPath := FLoader.MapItems.Add(TdxMapPath) as TdxMapPath;
  AMapPath.SegmentType := GetSegmentType;
  APartCount := GetPartCount(AShapeFileRecord);
  APointCount := GetPointCount(AShapeFileRecord);
  AMapPath.Segments.BeginUpdate;
  for I := 0 to APartCount - 1 do
  begin
    AMapPathSegment := AMapPath.Segments.Add;
    AEndPointIndex := IfThen(I < APartCount - 1, GetPart(AShapeFileRecord, I + 1) - 1, APointCount - 1);
    AMapPathSegment.GeoPoints.BeginUpdate;
    for J := GetPart(AShapeFileRecord, I) to AEndPointIndex do
      with AMapPathSegment.GeoPoints.Add do
        GeoPoint := GetPointLocation(AShapeFileRecord, J);
    AMapPathSegment.GeoPoints.EndUpdate;
  end;
  CreateAttributes(AMapPath, ADbfFileRecord);
  AMapPath.Segments.EndUpdate;
end;

function TdxMapControlShapeFilePolylineRecordConverter.GetPart(
  ARecord: TdxShapeFileRecord; AIndex: Integer): Integer;
begin
  Result := (ARecord as TdxShapeFilePolylineRecord).Parts[AIndex];
end;

function TdxMapControlShapeFilePolylineRecordConverter.GetPartCount(
  ARecord: TdxShapeFileRecord): Integer;
begin
  Result := (ARecord as TdxShapeFilePolylineRecord).PartCount;
end;

function TdxMapControlShapeFilePolylineRecordConverter.GetPointCount(
  ARecord: TdxShapeFileRecord): Integer;
begin
  Result := (ARecord as TdxShapeFilePolylineRecord).PointCount;
end;

function TdxMapControlShapeFilePolylineRecordConverter.GetPointLocation(
  ARecord: TdxShapeFileRecord; APointIndex: Integer): TdxMapControlGeoPoint;
begin
  with (ARecord as TdxShapeFilePolylineRecord).Points[APointIndex] do
    Result := dxMapControlGeoPoint(Y, X);
end;

class function TdxMapControlShapeFilePolylineRecordConverter.GetRecordClass: TdxShapeFileRecordClass;
begin
  Result := TdxShapeFilePolylineRecord;
end;

function TdxMapControlShapeFilePolylineRecordConverter.GetSegmentType: TdxMapSegmentType;
begin
  Result := mstPolyline;
end;

{ TdxMapControlShapeFilePolygonRecordConverter }

class function TdxMapControlShapeFilePolygonRecordConverter.GetRecordClass: TdxShapeFileRecordClass;
begin
  Result := TdxShapeFilePolygonRecord;
end;

function TdxMapControlShapeFilePolygonRecordConverter.GetSegmentType: TdxMapSegmentType;
begin
  Result := mstPolygon;
end;

{ TdxMapControlShapeFileMultiPointRecordConverter }

procedure TdxMapControlShapeFileMultiPointRecordConverter.Convert(
  AShapeFileRecord: TdxShapeFileRecord; ADbfFileRecord: TdxDbfFileRecord);
var
  AMapDot: TdxMapDot;
  I: Integer;
begin
  for I := 0 to GetPointCount(AShapeFileRecord) - 1 do
  begin
    AMapDot := FLoader.MapItems.Add(TdxMapDot) as TdxMapDot;
    AMapDot.Location.GeoPoint := GetPointLocation(AShapeFileRecord, I);
    CreateAttributes(AMapDot, ADbfFileRecord);
  end;
end;

function TdxMapControlShapeFileMultiPointRecordConverter.GetPointCount(ARecord: TdxShapeFileRecord): Integer;
begin
  Result := (ARecord as TdxShapeFileMultiPointRecord).PointCount;
end;

function TdxMapControlShapeFileMultiPointRecordConverter.GetPointLocation(
  ARecord: TdxShapeFileRecord; APointIndex: Integer): TdxMapControlGeoPoint;
begin
  with (ARecord as TdxShapeFileMultiPointRecord).Points[APointIndex] do
    Result := dxMapControlGeoPoint(Y, X);
end;

class function TdxMapControlShapeFileMultiPointRecordConverter.GetRecordClass: TdxShapeFileRecordClass;
begin
  Result := TdxShapeFileMultiPointRecord;
end;

{ TdxMapControlShapeFilePointMRecordConverter }

function TdxMapControlShapeFilePointMRecordConverter.GetPointLocation(
  ARecord: TdxShapeFileRecord): TdxMapControlGeoPoint;
var
  APointRecord: TdxShapeFilePointMRecord;
begin
  APointRecord := ARecord as TdxShapeFilePointMRecord;
  Result := dxMapControlGeoPoint(APointRecord.PointM.Y, APointRecord.PointM.X);
end;

class function TdxMapControlShapeFilePointMRecordConverter.GetRecordClass: TdxShapeFileRecordClass;
begin
  Result := TdxShapeFilePointMRecord;
end;

{ TdxMapControlShapeFileMultiPointMRecordConverter }

function TdxMapControlShapeFileMultiPointMRecordConverter.GetPointCount(
  ARecord: TdxShapeFileRecord): Integer;
begin
  Result := (ARecord as TdxShapeFileMultiPointMRecord).PointCount;
end;

function TdxMapControlShapeFileMultiPointMRecordConverter.GetPointLocation(
  ARecord: TdxShapeFileRecord; APointIndex: Integer): TdxMapControlGeoPoint;
begin
  with (ARecord as TdxShapeFileMultiPointMRecord).PointsM[APointIndex] do
    Result := dxMapControlGeoPoint(Y, X);
end;

class function TdxMapControlShapeFileMultiPointMRecordConverter.GetRecordClass: TdxShapeFileRecordClass;
begin
  Result := TdxShapeFileMultiPointMRecord;
end;

{ TdxMapControlShapeFilePolylineMRecordConverter }

function TdxMapControlShapeFilePolylineMRecordConverter.GetPart(
  ARecord: TdxShapeFileRecord; AIndex: Integer): Integer;
begin
  Result := (ARecord as TdxShapeFilePolylineMRecord).Parts[AIndex];
end;

function TdxMapControlShapeFilePolylineMRecordConverter.GetPartCount(
  ARecord: TdxShapeFileRecord): Integer;
begin
  Result := (ARecord as TdxShapeFilePolylineMRecord).PartCount;
end;

function TdxMapControlShapeFilePolylineMRecordConverter.GetPointCount(
  ARecord: TdxShapeFileRecord): Integer;
begin
  Result := (ARecord as TdxShapeFilePolylineMRecord).PointCount;
end;

function TdxMapControlShapeFilePolylineMRecordConverter.GetPointLocation(
  ARecord: TdxShapeFileRecord; APointIndex: Integer): TdxMapControlGeoPoint;
begin
  with (ARecord as TdxShapeFilePolylineMRecord).PointsM[APointIndex] do
    Result := dxMapControlGeoPoint(Y, X);
end;

class function TdxMapControlShapeFilePolylineMRecordConverter.GetRecordClass: TdxShapeFileRecordClass;
begin
  Result := TdxShapeFilePolylineMRecord;
end;

{ TdxMapControlShapeFilePolygonMRecordConverter }

function TdxMapControlShapeFilePolygonMRecordConverter.GetPart(
  ARecord: TdxShapeFileRecord; AIndex: Integer): Integer;
begin
  Result := (ARecord as TdxShapeFilePolygonMRecord).Parts[AIndex];
end;

function TdxMapControlShapeFilePolygonMRecordConverter.GetPartCount(
  ARecord: TdxShapeFileRecord): Integer;
begin
  Result := (ARecord as TdxShapeFilePolygonMRecord).PartCount;
end;

function TdxMapControlShapeFilePolygonMRecordConverter.GetPointCount(
  ARecord: TdxShapeFileRecord): Integer;
begin
  Result := (ARecord as TdxShapeFilePolygonMRecord).PointCount;
end;

function TdxMapControlShapeFilePolygonMRecordConverter.GetPointLocation(
  ARecord: TdxShapeFileRecord; APointIndex: Integer): TdxMapControlGeoPoint;
begin
  with (ARecord as TdxShapeFilePolygonMRecord).PointsM[APointIndex] do
    Result := dxMapControlGeoPoint(Y, X);
end;

class function TdxMapControlShapeFilePolygonMRecordConverter.GetRecordClass: TdxShapeFileRecordClass;
begin
  Result := TdxShapeFilePolygonMRecord;
end;

initialization
  dxMapControlShapeFileRecordsConverters.RegisterConverters([TdxMapControlShapeFilePointRecordConverter,
    TdxMapControlShapeFileMultiPointRecordConverter, TdxMapControlShapeFilePolylineRecordConverter,
    TdxMapControlShapeFilePolygonRecordConverter, TdxMapControlShapeFilePointMRecordConverter,
    TdxMapControlShapeFileMultiPointMRecordConverter, TdxMapControlShapeFilePolylineMRecordConverter,
    TdxMapControlShapeFilePolygonMRecordConverter]);

finalization
  FreeAndNil(FShapeFileRecordsConverterFactory);

end.
