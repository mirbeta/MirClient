//---------------------------------------------------------------------------

#ifndef RealtorWorldDMH
#define RealtorWorldDMH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <DB.hpp>
#include <DBClient.hpp>
#include <Provider.hpp>
//---------------------------------------------------------------------------
class TDMRealtorWorld : public TDataModule
{
__published:	// IDE-managed Components
	TDataSource *dsHomePhotos;
	TDataSource *dsHomesAndAgents;
	TDataSource *dsHouseSales;
	TDataSource *dsMortgage;
	TDataSource *dsHomesAndHomes;
	TClientDataSet *clMortgage;
	TDateTimeField *clMortgageDate1;
	TFloatField *clMortgageFRM30;
	TFloatField *clMortgageFRM15;
	TFloatField *clMortgageARM1;
	TClientDataSet *clHousesSales;
	TIntegerField *clHousesSalesID;
	TDateTimeField *clHousesSalesDate1;
	TIntegerField *clHousesSalesCount1;
	TMemoField *clHousesSalesRegion;
	TMemoField *clHousesSalesSeasonallyAdjusted;
	TMemoField *clHousesSalesType1;
	TClientDataSet *clHomesAndHomes;
	TIntegerField *clHomesAndHomesID;
	TMemoField *clHomesAndHomesAddress;
	TSmallintField *clHomesAndHomesBeds;
	TSmallintField *clHomesAndHomesBaths;
	TFloatField *clHomesAndHomesHouseSize;
	TFloatField *clHomesAndHomesLotSize;
	TFloatField *clHomesAndHomesPrice;
	TMemoField *clHomesAndHomesFeatures;
	TMemoField *clHomesAndHomesYearBuilt;
	TIntegerField *clHomesAndHomesType;
	TIntegerField *clHomesAndHomesStatus;
	TBlobField *clHomesAndHomesPhoto;
	TIntegerField *clHomesAndHomesAgentID;
	TIntegerField *clHomesAndHomesYearID;
	TClientDataSet *clHomesAndAgents;
	TClientDataSet *clHomePhotos;
	TDataSetProvider *dspHomesAndHomes;
	TDataSource *dsHomesDetail;
	TClientDataSet *clHomesDetail;
	TIntegerField *clHomesDetailID;
	TMemoField *clHomesDetailAddress;
	TSmallintField *clHomesDetailBeds;
	TSmallintField *clHomesDetailBaths;
	TFloatField *clHomesDetailHouseSize;
	TFloatField *clHomesDetailLotSize;
	TFloatField *clHomesDetailPrice;
	TMemoField *clHomesDetailFeatures;
	TMemoField *clHomesDetailYearBuilt;
	TIntegerField *clHomesDetailType;
	TIntegerField *clHomesDetailStatus;
	TBlobField *clHomesDetailPhoto;
	TIntegerField *clHomesDetailAgentID;
	TIntegerField *clHomesDetailYearID;
	TDataSource *dsHouseSalsesChart;
	TClientDataSet *clHouseSalesChart;
	TDateField *clHouseSalesChartDate;
	TStringField *clHouseSalesChartState;
	TStringField *clHouseSalesChartSeasonallyAdjusted;
	TIntegerField *clHouseSalesChartMidWest;
	TIntegerField *clHouseSalesChartNorthEast;
	TIntegerField *clHouseSalesChartSouth;
	TIntegerField *clHouseSalesChartWest;
	TIntegerField *clHouseSalesChartAll;
	TClientDataSet *clHouseRating;
	TIntegerField *clHouseRatingHouseID;
	TStringField *clHouseRatingRegionName;
	TIntegerField *clHouseRatingValue;
	TDataSource *dsHouseRating;
	TDataSource *dsHousesSimular;
	TClientDataSet *clHousesSimular;
	TIntegerField *IntegerField1;
	TIntegerField *clHousesSimularYear;
	TIntegerField *clHousesSimularProposals;
	TIntegerField *clHousesSimularSold;
	TDataSource *dsHousePrice;
	TClientDataSet *clHousePrice;
	TIntegerField *IntegerField2;
	TDateField *clHousePriceDate;
	TFloatField *clHousePricePrice;
	TDataSource *dsResearchChart;
	TClientDataSet *clResearchChart;
	TDateField *clResearchChartDate;
	TStringField *clResearchChartState;
	TStringField *clResearchChartSeasonallyAdjusted;
	TIntegerField *clResearchChartMidWest;
	TIntegerField *clResearchChartNorthEast;
	TIntegerField *clResearchChartSouth;
	TIntegerField *clResearchChartWest;
	TIntegerField *clResearchChartAll;
	void __fastcall DataModuleCreate(TObject *Sender);
	void __fastcall clHomesAndHomesCalcFields(TDataSet *DataSet);
private:	// User declarations
public:		// User declarations
	__fastcall TDMRealtorWorld(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TDMRealtorWorld *DMRealtorWorld;
//---------------------------------------------------------------------------
#endif
