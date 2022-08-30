//---------------------------------------------------------------------------

#ifndef CarsDataH
#define CarsDataH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include <DB.hpp>
#include "dxmdaset.hpp"
#include "cxClasses.hpp"
#include "cxEdit.hpp"
#include "cxEditRepositoryItems.hpp"
#include "cxDBEditRepository.hpp"
#include "cxStyles.hpp"
#include "cxCustomData.hpp" 
#include "cxGraphics.hpp"
#include "cxFilter.hpp"
#include "cxData.hpp"
#include "cxDataStorage.hpp"
#include "cxNavigator.hpp"
#include "cxControls.hpp"
#include "cxHyperLinkEdit.hpp"
#include "cxDBData.hpp"
#include "cxMemo.hpp"
  
//---------------------------------------------------------------------------
class TdmCars : public TDataModule
{
__published:  // IDE-managed Components
    TDataSource* dsBodyStyle;
    TDataSource* dsCategory;
    TDataSource* dsModels;
    TDataSource* dsTrademark;
    TDataSource* dsTransmissionType;
    TcxEditRepository* EditRepository;
    TcxEditRepositoryLookupComboBoxItem* EditRepositoryBodyStyleLookup;
    TcxEditRepositoryLookupComboBoxItem* EditRepositoryCategoryLookup;
    TcxEditRepositoryImageItem* EditRepositoryImage;
    TcxEditRepositoryBlobItem* EditRepositoryImageBlob;
    TcxEditRepositoryMemoItem* EditRepositoryMemo;
    TcxEditRepositoryBlobItem* EditRepositoryMemoBlob;
    TcxEditRepositoryCheckBoxItem* EditRepositoryTransmissionTypeCheckBox;
    TcxEditRepositoryLookupComboBoxItem* EditRepositoryTransmissionTypeLookup;
    TdxMemData* mdBodyStyle;
    TIntegerField* mdBodyStyleID;
    TWideStringField* mdBodyStyleName;
    TdxMemData* mdCategory;
    TIntegerField* mdCategoryID;
    TWideStringField* mdCategoryName;
    TBlobField* mdCategoryPicture;
    TdxMemData* mdModels;
    TIntegerField* mdModelsBodyStyleID;
    TIntegerField* mdModelsCategoryID;
    TIntegerField* mdModelsCilinders;
    TDateTimeField* mdModelsDelivery_Date;
    TWideMemoField* mdModelsDescription;
    TIntegerField* mdModelsDoors;
    TWideStringField* mdModelsFullName;
    TWideStringField* mdModelsHorsepower;
    TStringField* mdModelsHyperlink;
    TIntegerField* mdModelsID;
    TBlobField* mdModelsImage;
    TBooleanField* mdModelsInStock;
    TWideStringField* mdModelsModification;
    TIntegerField* mdModelsMPG_City;
    TIntegerField* mdModelsMPG_Highway;
    TWideStringField* mdModelsName;
    TBlobField* mdModelsPhoto;
    TBCDField* mdModelsPrice;
    TWideStringField* mdModelsTorque;
    TWideStringField* mdModelsTrademark;
    TIntegerField* mdModelsTrademarkID;
    TWideStringField* mdModelsTransmission_Speeds;
    TIntegerField* mdModelsTransmission_Type;
    TStringField* mdModelsTransmissionTypeName;
    TdxMemData* mdTrademark;
    TWideMemoField* mdTrademarkDescription;
    TIntegerField* mdTrademarkID;
    TBlobField* mdTrademarkLogo;
    TWideStringField* mdTrademarkName;
    TWideStringField* mdTrademarkSite;
    TdxMemData* mdTransmissionType;
    TIntegerField* mdTransmissionTypeID;
    TWideStringField* mdTransmissionTypeName;
    TStringField* mdModelsCategory;
    TStringField* mdModelsBodyStyle;
    TDataSource* dsCarOrders;
    TdxMemData* mdCarOrders;
    TIntegerField* mdCarOrdersID;
    TIntegerField* mdCarOrdersParentID;
    TWideStringField* mdCarOrdersName;
    TWideStringField* mdCarOrdersModification;
    TBCDField* mdCarOrdersPrice;
    TIntegerField* mdCarOrdersMPG_City;
    TIntegerField* mdCarOrdersMPG_Highway;
    TIntegerField* mdCarOrdersBodyStyleID;
    TIntegerField* mdCarOrdersCilinders;
    TDateField* mdCarOrdersSalesDate;
    TStringField* mdCarOrdersBodyStyle;

    void __fastcall DataModuleCreate(TObject* Sender);
    void __fastcall mdModelsCalcFields(TDataSet* DataSet);	
public:   // User declarations
	__fastcall TdmCars(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TdmCars *dmCars;
//---------------------------------------------------------------------------
#endif
