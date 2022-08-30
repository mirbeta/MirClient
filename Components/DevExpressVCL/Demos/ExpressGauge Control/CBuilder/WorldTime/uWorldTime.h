//---------------------------------------------------------------------------

#ifndef uWorldTimeH
#define uWorldTimeH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxLabel.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxSplitter.hpp"
#include "dxGaugeCircularScale.hpp"
#include "dxGaugeControl.hpp"
#include "dxGaugeCustomScale.hpp"
#include "dxGaugeDigitalScale.hpp"
#include "dxGaugeQuantitativeScale.hpp"
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
  struct TTimeInfo
  {
	Word Hour;
	Word Min;
	Word Sec;
  };
class TfrmWorldTime : public TForm
{
__published:	// IDE-managed Components
	TPanel *pnlClocks;
	TPanel *pnlWashingtonClock;
	TdxGaugeControl *gcWashingtonTime;
	TdxGaugeCircularScale *gsWashingtonTimeBackgroundLayer;
	TdxGaugeCircularScale *gsWashingtonTimeSecondNeedle;
	TdxGaugeCircularScale *gsWashingtonTimeHourNeedle;
	TdxGaugeCircularScale *gsWashingtonTimeMinuteNeedle;
	TcxLabel *cxLabel1;
	TPanel *pnlParisClock;
	TdxGaugeControl *gcParisTime;
	TdxGaugeCircularScale *dxGaugeCircularScale1;
	TdxGaugeCircularScale *dxGaugeCircularScale2;
	TdxGaugeCircularScale *dxGaugeCircularScale3;
	TdxGaugeCircularScale *dxGaugeCircularScale4;
	TcxLabel *cxLabel3;
	TPanel *pnlMoscowClock;
	TdxGaugeControl *gcMoscowTime;
	TdxGaugeCircularScale *dxGaugeCircularScale5;
	TdxGaugeCircularScale *dxGaugeCircularScale6;
	TdxGaugeCircularScale *dxGaugeCircularScale7;
	TdxGaugeCircularScale *dxGaugeCircularScale8;
	TcxLabel *cxLabel2;
	TPanel *pnlLocalTime;
	TdxGaugeControl *gcLocalTime;
	TdxGaugeDigitalScale *gsDigital;
	TcxSplitter *cxSplitter1;
	TcxLabel *lbDescription;
	TTimer *Timer1;
	TcxLookAndFeelController *cxLookAndFeelController1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall FormShow(TObject *Sender);
	void __fastcall pnlClocksResize(TObject *Sender);
private:	// User declarations
	TTimeInfo GetTimeInfo(int ATimeZone);
	void UpdateClock(TdxGaugeControl* AGaugeControl, TTimeInfo ATimeInfo);
	void UpdateClocks();
	void CalculateGaugeBounds();
public:		// User declarations
	__fastcall TfrmWorldTime(TComponent* Owner);
};

//---------------------------------------------------------------------------
extern PACKAGE TfrmWorldTime *frmWorldTime;
//---------------------------------------------------------------------------
#endif
