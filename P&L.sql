 USE H_Accounting;
 
 DROP PROCEDURE IF EXISTS `gghedini_team1`;
 
 DELIMITER $$
	CREATE PROCEDURE `gghedini_team1`(IN varCalendarYear SMALLINT, OUT net_income DOUBLE)
	BEGIN
			DECLARE varRevenueThisYear DOUBLE DEFAULT 0;
			DECLARE varRevenueLastYear DOUBLE DEFAULT 0;
			DECLARE varCOGSThisyear DOUBLE DEFAULT 0;
            DECLARE varCOGSLastyear DOUBLE DEFAULT 0;
            DECLARE varRETThisyear DOUBLE DEFAULT 0;
            DECLARE varRETLastyear DOUBLE DEFAULT 0;
            DECLARE varGEXPThisyear DOUBLE DEFAULT 0;
            DECLARE varGEXPLastyear DOUBLE DEFAULT 0;
            DECLARE varSEXPThisyear DOUBLE DEFAULT 0;
            DECLARE varSEXPLastyear DOUBLE DEFAULT 0;
            DECLARE varOEXPThisyear DOUBLE DEFAULT 0;
			DECLARE varOEXPLastyear DOUBLE DEFAULT 0;
            DECLARE varOIThisYear DOUBLE DEFAULT 0;
            DECLARE varOILastYear DOUBLE DEFAULT 0;
            DECLARE varINCTAXThisYear DOUBLE DEFAULT 0;
            DECLARE varOTHTAXThisYear DOUBLE DEFAULT 0;
            
	SELECT SUM(jeli.credit) INTO varRevenueThisYear
    FROM journal_entry_line_item AS jeli
	
		INNER JOIN account AS ac ON ac.account_id = jeli.account_id
		INNER JOIN journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
		INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.profit_loss_section_id
      
    WHERE ss.statement_section_code = "REV"
	  AND YEAR(je.entry_date) = varCalendarYear
	;
    
    SELECT SUM(jeli.credit) INTO varRevenueLastYear
    FROM journal_entry_line_item AS jeli
    
		INNER JOIN account AS ac ON ac.account_id = jeli.account_id
		INNER JOIN journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
		INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.profit_loss_section_id
        
	WHERE ss.statement_section_code = "REV"
	  AND YEAR(je.entry_date) = varCalendarYear - 1
	;
    
    SELECT SUM(jeli.debit) INTO varCOGSThisyear
    
	FROM journal_entry_line_item AS jeli
		INNER JOIN account AS a USING(account_id)
		INNER JOIN journal_entry AS j USING(journal_entry_id)
        INNER JOIN statement_section AS ss ON ss.statement_section_id = a.profit_loss_section_id
	WHERE ss.statement_section_code = "COGS" 
		AND YEAR(j.entry_date) = varCalendarYear
    ;
    
       SELECT SUM(jeli.debit) INTO varCOGSLastyear
    
	FROM journal_entry_line_item AS jeli
		INNER JOIN account AS a USING(account_id)
		INNER JOIN journal_entry AS j USING(journal_entry_id)
        INNER JOIN statement_section AS ss ON ss.statement_section_id = a.profit_loss_section_id
	WHERE ss.statement_section_code = "COGS" 
		AND YEAR(j.entry_date) = varCalendarYear-1
    ;
    
		SELECT COALESCE(SUM(jeli.debit),0) INTO varRETThisyear -- COALESCE returns 0 if the result is NULL
    
	FROM journal_entry_line_item AS jeli
		INNER JOIN account AS a USING(account_id)
		INNER JOIN journal_entry AS j USING(journal_entry_id)
        INNER JOIN statement_section AS ss ON ss.statement_section_id = a.profit_loss_section_id
	WHERE ss.statement_section_code = "RET" 
		AND YEAR(j.entry_date) = varCalendarYear
    ;
		
		SELECT COALESCE(SUM(jeli.debit),0) INTO varRETLastyear
	FROM journal_entry_line_item AS jeli
		INNER JOIN account AS a USING(account_id)
		INNER JOIN journal_entry AS j USING(journal_entry_id)
        INNER JOIN statement_section AS ss ON ss.statement_section_id = a.profit_loss_section_id
	WHERE ss.statement_section_code = "RET" 
		AND YEAR(j.entry_date) = varCalendarYear-1
    ;
       
       SELECT COALESCE(SUM(jeli.debit),0) INTO varGEXPThisyear -- COALESCE returns 0 if the result is NULL
    
	FROM journal_entry_line_item AS jeli
		INNER JOIN account AS a USING(account_id)
		INNER JOIN journal_entry AS j USING(journal_entry_id)
        INNER JOIN statement_section AS ss ON ss.statement_section_id = a.profit_loss_section_id
	WHERE ss.statement_section_code = "GEXP" 
		AND YEAR(j.entry_date) = varCalendarYear
    ;
		SELECT COALESCE(SUM(jeli.debit),0) INTO varGEXPLastyear
	FROM journal_entry_line_item AS jeli
		INNER JOIN account AS a USING(account_id)
		INNER JOIN journal_entry AS j USING(journal_entry_id)
        INNER JOIN statement_section AS ss ON ss.statement_section_id = a.profit_loss_section_id
	WHERE ss.statement_section_code = "GEXP" 
		AND YEAR(j.entry_date) = varCalendarYear-1
    ;
    
         SELECT COALESCE(SUM(jeli.debit),0) INTO varSEXPThisyear
    
	FROM journal_entry_line_item AS jeli
		INNER JOIN account AS a USING(account_id)
		INNER JOIN journal_entry AS j USING(journal_entry_id)
        INNER JOIN statement_section AS ss ON ss.statement_section_id = a.profit_loss_section_id
	WHERE ss.statement_section_code = "SEXP" 
		AND YEAR(j.entry_date) = varCalendarYear
    ;
    SELECT COALESCE(SUM(jeli.debit),0) INTO varSEXPLastyear
	FROM journal_entry_line_item AS jeli
		INNER JOIN account AS a USING(account_id)
		INNER JOIN journal_entry AS j USING(journal_entry_id)
        INNER JOIN statement_section AS ss ON ss.statement_section_id = a.profit_loss_section_id
	WHERE ss.statement_section_code = "SEXP" 
		AND YEAR(j.entry_date) = varCalendarYear-1
    ;
    
		SELECT COALESCE(SUM(jeli.debit),0) INTO varOEXPThisyear
    
	FROM journal_entry_line_item AS jeli
		INNER JOIN account AS a USING(account_id)
		INNER JOIN journal_entry AS j USING(journal_entry_id)
        INNER JOIN statement_section AS ss ON ss.statement_section_id = a.profit_loss_section_id
	WHERE ss.statement_section_code = "OEXP" 
		AND YEAR(j.entry_date) = varCalendarYear
    ;
    SELECT COALESCE(SUM(jeli.debit),0) INTO varOEXPLastyear
	FROM journal_entry_line_item AS jeli
		INNER JOIN account AS a USING(account_id)
		INNER JOIN journal_entry AS j USING(journal_entry_id)
        INNER JOIN statement_section AS ss ON ss.statement_section_id = a.profit_loss_section_id
	WHERE ss.statement_section_code = "OEXP" 
		AND YEAR(j.entry_date) = varCalendarYear-1
    ;

		SELECT COALESCE(SUM(jeli.credit),0) INTO varOIThisYear
	FROM journal_entry_line_item AS jeli
		INNER JOIN account AS a USING(account_id)
		INNER JOIN journal_entry AS j USING(journal_entry_id)
        INNER JOIN statement_section AS ss ON ss.statement_section_id = a.profit_loss_section_id
	WHERE ss.statement_section_code= "OI" 
		AND YEAR(j.entry_date) = varCalendarYear
    ;
    
    	SELECT COALESCE(SUM(jeli.credit),0) INTO varOILastYear
	FROM journal_entry_line_item AS jeli
		INNER JOIN account AS a USING(account_id)
		INNER JOIN journal_entry AS j USING(journal_entry_id)
        INNER JOIN statement_section AS ss ON ss.statement_section_id = a.profit_loss_section_id
	WHERE ss.statement_section_code= "OI" 
		AND YEAR(j.entry_date) = varCalendarYear-1
    ;

   	SELECT COALESCE(SUM(jeli.credit),0) INTO varINCTAXThisYear
	FROM journal_entry_line_item AS jeli
		INNER JOIN account AS a USING(account_id)
		INNER JOIN journal_entry AS j USING(journal_entry_id)
        INNER JOIN statement_section AS ss ON ss.statement_section_id = a.profit_loss_section_id
	WHERE ss.statement_section_code= "INCTAX" 
		AND YEAR(j.entry_date) = varCalendarYear
    ;
    
	SELECT COALESCE(SUM(jeli.credit),0) INTO varOTHTAXThisYear
	FROM journal_entry_line_item AS jeli
		INNER JOIN account AS a USING(account_id)
		INNER JOIN journal_entry AS j USING(journal_entry_id)
        INNER JOIN statement_section AS ss ON ss.statement_section_id = a.profit_loss_section_id
	WHERE ss.statement_section_code= "OTHTAX" 
		AND YEAR(j.entry_date) = varCalendarYear
    ;


    	-- DROP THE TMP, TO INSERT THE VALUES INTO THE TMP FORMATTED
	DROP TABLE IF EXISTS gghedini_tmp;
     
	-- WE CALCULATE THE GROWTH ON REVENUE vs. LAST YEAR
    CREATE TABLE gghedini_tmp AS
    -- SELECT "label", "amount (thousands $)", "last-year amount", "% change year over year";
    
    -- INSERT INTO gghedini_tmp
    SELECT 	"Revenue                           " AS "labels" ,
			FORMAT(varRevenueThisYear, 1) AS "amount", 
			-- FORMAT(varRevenueLastYear, 1), 
            FORMAT((varRevenueThisYear / varRevenueLastYear - 1) * 100, 1) AS "% change YoY"
		;
	
    INSERT INTO gghedini_tmp
    SELECT 	" - COGS",
			FORMAT(varCOGSThisYear, 1), 
			-- FORMAT(varCOGSLastYear, 1), 
            FORMAT((varCOGSThisYear / varCOGSLastYear - 1) * 100, 1)
		;
	
    INSERT INTO gghedini_tmp
    SELECT 	"Gross Profit",
			FORMAT(varRevenueThisYear - varCOGSThisYear, 1),
			-- FORMAT(varRevenueLastYear - varCOGSLastYear, 1), "",
            FORMAT(((varRevenueThisYear-varCOGSThisYear)-(varRevenueLastYear-varCOGSLastYear)) / (varRevenueLastYear-varCOGSLastYear)*100, 2)
		;
        
	INSERT INTO gghedini_tmp 
	SELECT " ", "",""
    ;
	
    INSERT INTO gghedini_tmp
    SELECT " - Return/Refound/Discount",
			FORMAT(varRETThisyear,1),""
		;
    
    INSERT INTO gghedini_tmp
    SELECT 	" - Admin. Expenses",
			FORMAT(varGEXPThisYear, 1),""
		;
        
	INSERT INTO gghedini_tmp
    SELECT 	" - Selling Expenses",
			FORMAT(varSEXPThisYear, 1),""
		;
        
    INSERT INTO gghedini_tmp
    SELECT 	" - Other Expenses",
			FORMAT(varOEXPThisYear, 1),""
		;
	
    INSERT INTO gghedini_tmp
    SELECT "Total Operating Expenses",
			FORMAT(varOEXPThisYear + varSEXPThisYear + varGEXPThisYear + varRETThisyear,2),
			FORMAT(((varOEXPThisYear + varSEXPThisYear + varGEXPThisYear + varRETThisYear) - (varOEXPLastYear + varSEXPLastYear + varGEXPLastYear + varRETLastYear)) / (varOEXPLastYear + varSEXPLastYear + varGEXPLastYear + varRETLastYear) * 100, 2)
		;
        
	INSERT INTO gghedini_tmp 
	SELECT " ", "",""
    ;
    
    INSERT INTO gghedini_tmp
    SELECT "Other Income ",
			FORMAT(varOIThisYear,1),""
    ;
    
    INSERT INTO gghedini_tmp
    SELECT "EBIT ",
			FORMAT((varRevenueThisYear - varCOGSThisYear) - (varOEXPThisYear + varSEXPThisYear + varGEXPThisYear + varRETThisyear) + varOIThisYear,1),
            FORMAT((((varRevenueThisYear - varCOGSThisYear) - (varOEXPThisYear + varSEXPThisYear + varGEXPThisYear + varRETThisyear) + varOIThisYear) - ((varRevenueLastYear - varCOGSLastYear) - (varOEXPLastYear + varSEXPLastYear + varGEXPLastYear + varRETLastyear) + varOILastYear)) / ((varOEXPLastYear + varSEXPLastYear + varGEXPLastYear + varRETLastyear) + varOILastYear)*100,2)
    ;
    
	INSERT INTO gghedini_tmp
    SELECT "Income TAX ",
			FORMAT(varINCTAXThisYear,1),""
    ;
    
    INSERT INTO gghedini_tmp
    SELECT "Other TAX ",
			FORMAT(varOTHTAXThisYear,1),""
    ;
    
    INSERT INTO gghedini_tmp
    SELECT "NET INCOME ",
			FORMAT(((varRevenueThisYear - varCOGSThisYear) - (varOEXPThisYear + varSEXPThisYear + varGEXPThisYear + varRETThisyear) + varOIThisYear)-varOTHTAXThisYear-varINCTAXThisYear,1),""
    ;
    SET net_income = ((varRevenueThisYear - varCOGSThisYear) - (varOEXPThisYear + varSEXPThisYear + varGEXPThisYear + varRETThisyear) + varOIThisYear)-varOTHTAXThisYear-varINCTAXThisYear
    ;
    
END $$
DELIMITER ;

#########################

 DROP PROCEDURE IF EXISTS `gghedini_balance_sheet`;

-- The tpycal delimiter for Stored procedures is a double dollar sign
DELIMITER $$
	CREATE PROCEDURE `gghedini_balance_sheet`(varCalendarYear SMALLINT)
	BEGIN

	DECLARE varCurrentAssetsThisYear DOUBLE DEFAULT 0;
    DECLARE varCurrentAssetsLastYear DOUBLE DEFAULT 0;
	DECLARE varFixAssetsThisYear DOUBLE DEFAULT 0;
    DECLARE varFixAssetsLastYear DOUBLE DEFAULT 0;
	DECLARE varDeferredAssetsThisYear DOUBLE DEFAULT 0;
    DECLARE varDeferredAssetsLastYear DOUBLE DEFAULT 0;
	DECLARE varCurrentLiabilitiesThisYear DOUBLE DEFAULT 0;
    DECLARE varCurrentLiabilitiesLastYear DOUBLE DEFAULT 0;
	DECLARE varLongtermLiabilitiesThisYear DOUBLE DEFAULT 0;
    DECLARE varLongtermLiabilitiesLastYear DOUBLE DEFAULT 0;
	DECLARE varDeferredLiabilitiesThisYear DOUBLE DEFAULT 0;
    DECLARE varDeferredLiabilitiesLastYear DOUBLE DEFAULT 0;
    DECLARE varEquityThisYear DOUBLE DEFAULT 0;
    DECLARE varEquityLastYear DOUBLE DEFAULT 0;
	CALL gghedini_team1(@varYear, @net_income);

SELECT 
    COALESCE(SUM(jeli.debit),0) - COALESCE(SUM(jeli.credit),0)
INTO varCurrentAssetsThisYear FROM
    journal_entry_line_item AS jeli
        INNER JOIN
    account AS ac ON ac.account_id = jeli.account_id
        INNER JOIN
    journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
        INNER JOIN
    statement_section AS ss ON ss.statement_section_id = ac.balance_sheet_section_id
WHERE
    ss.statement_section_code = 'CA'
        AND YEAR(je.entry_date) <= varCalendarYear
        AND je.cancelled = 0
	;
    
SELECT 
COALESCE(SUM(jeli.debit),0) - COALESCE(SUM(jeli.credit),0)
INTO varCurrentAssetsLastYear FROM
    journal_entry_line_item AS jeli
        INNER JOIN
    account AS ac ON ac.account_id = jeli.account_id
        INNER JOIN
    journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
        INNER JOIN
    statement_section AS ss ON ss.statement_section_id = ac.balance_sheet_section_id
WHERE
    ss.statement_section_code = 'CA'
        AND YEAR(je.entry_date) <= varCalendarYear - 1
        AND je.cancelled = 0
	;

	SELECT 
 COALESCE(SUM(jeli.debit),0) - COALESCE(SUM(jeli.credit),0)
INTO varFixAssetsThisYear FROM
    journal_entry_line_item AS jeli
        INNER JOIN
    account AS ac ON ac.account_id = jeli.account_id
        INNER JOIN
    journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
        INNER JOIN
    statement_section AS ss ON ss.statement_section_id = ac.balance_sheet_section_id
WHERE
    ss.statement_section_code = 'FA'
        AND YEAR(je.entry_date) <= varCalendarYear
        AND je.cancelled = 0
	;
    
SELECT 
COALESCE(SUM(jeli.debit),0) - COALESCE(SUM(jeli.credit),0)
INTO varFixAssetsLastYear FROM
    journal_entry_line_item AS jeli
        INNER JOIN
    account AS ac ON ac.account_id = jeli.account_id
        INNER JOIN
    journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
        INNER JOIN
    statement_section AS ss ON ss.statement_section_id = ac.balance_sheet_section_id
WHERE
    ss.statement_section_code = 'FA'
        AND YEAR(je.entry_date) <= varCalendarYear - 1
        AND je.cancelled = 0
	;

	SELECT 
COALESCE(SUM(jeli.debit),0) - COALESCE(SUM(jeli.credit),0)
INTO varDeferredAssetsThisYear FROM
    journal_entry_line_item AS jeli
        INNER JOIN
    account AS ac ON ac.account_id = jeli.account_id
        INNER JOIN
    journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
        INNER JOIN
    statement_section AS ss ON ss.statement_section_id = ac.balance_sheet_section_id
WHERE
    ss.statement_section_code = 'DA'
        AND YEAR(je.entry_date) <= varCalendarYear
        AND je.cancelled = 0
	;
    
SELECT 
COALESCE(SUM(jeli.debit),0) - COALESCE(SUM(jeli.credit),0)
INTO varDeferredAssetsLastYear FROM
    journal_entry_line_item AS jeli
        INNER JOIN
    account AS ac ON ac.account_id = jeli.account_id
        INNER JOIN
    journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
        INNER JOIN
    statement_section AS ss ON ss.statement_section_id = ac.balance_sheet_section_id
WHERE
    ss.statement_section_code = 'DA'
        AND YEAR(je.entry_date) <= varCalendarYear - 1
        AND je.cancelled = 0
;

SELECT 
    COALESCE(SUM(jeli.credit),0) - COALESCE(SUM(jeli.debit),0)
INTO varCurrentLiabilitiesThisYear FROM
    journal_entry_line_item AS jeli
        INNER JOIN
    account AS ac ON ac.account_id = jeli.account_id
        INNER JOIN
    journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
        INNER JOIN
    statement_section AS ss ON ss.statement_section_id = ac.balance_sheet_section_id
WHERE
    ss.statement_section_code = 'CL'
        AND YEAR(je.entry_date) <= varCalendarYear
        AND je.cancelled = 0
	;

SELECT 
    COALESCE(SUM(jeli.credit),0) - COALESCE(SUM(jeli.debit),0)
INTO varCurrentLiabilitiesLastYear FROM
    journal_entry_line_item AS jeli
        INNER JOIN
    account AS ac ON ac.account_id = jeli.account_id
        INNER JOIN
    journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
        INNER JOIN
    statement_section AS ss ON ss.statement_section_id = ac.balance_sheet_section_id
WHERE
    ss.statement_section_code = 'CL'
        AND YEAR(je.entry_date) <= varCalendarYear - 1
        AND je.cancelled = 0
	;

SELECT 
    COALESCE(SUM(jeli.credit),0) - COALESCE(SUM(jeli.debit),0)
INTO varLongtermLiabilitiesThisYear FROM
    journal_entry_line_item AS jeli
        INNER JOIN
    account AS ac ON ac.account_id = jeli.account_id
        INNER JOIN
    journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
        INNER JOIN
    statement_section AS ss ON ss.statement_section_id = ac.balance_sheet_section_id
WHERE
    ss.statement_section_code = 'LLL'
        AND YEAR(je.entry_date) <= varCalendarYear
        AND je.cancelled = 0
	;

SELECT 
    COALESCE(SUM(jeli.credit),0) - COALESCE(SUM(jeli.debit),0)
INTO varLongtermLiabilitiesLastYear FROM
    journal_entry_line_item AS jeli
        INNER JOIN
    account AS ac ON ac.account_id = jeli.account_id
        INNER JOIN
    journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
        INNER JOIN
    statement_section AS ss ON ss.statement_section_id = ac.balance_sheet_section_id
WHERE
    ss.statement_section_code = 'LLL'
        AND YEAR(je.entry_date) <= varCalendarYear -1
        AND je.cancelled = 0
	;

SELECT 
     COALESCE(SUM(jeli.credit),0) - COALESCE(SUM(jeli.debit),0)
INTO varDeferredLiabilitiesThisYear FROM
    journal_entry_line_item AS jeli
        INNER JOIN
    account AS ac ON ac.account_id = jeli.account_id
        INNER JOIN
    journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
        INNER JOIN
    statement_section AS ss ON ss.statement_section_id = ac.balance_sheet_section_id
WHERE
    ss.statement_section_code = 'DL'
        AND YEAR(je.entry_date) <= varCalendarYear
        AND je.cancelled = 0
	;


SELECT 
       COALESCE(SUM(jeli.credit),0) - COALESCE(SUM(jeli.debit),0)
INTO varDeferredLiabilitiesLastYear FROM
    journal_entry_line_item AS jeli
        INNER JOIN
    account AS ac ON ac.account_id = jeli.account_id
        INNER JOIN
    journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
        INNER JOIN
    statement_section AS ss ON ss.statement_section_id = ac.balance_sheet_section_id
WHERE
    ss.statement_section_code = 'DL'
        AND YEAR(je.entry_date) <= varCalendarYear - 1
        AND je.cancelled = 0
	;


SELECT 
    COALESCE(SUM(jeli.credit),0) - COALESCE(SUM(jeli.debit),0) INTO varEquityThisYear
FROM
    journal_entry_line_item AS jeli
        INNER JOIN
    account AS ac ON ac.account_id = jeli.account_id
        INNER JOIN
    journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
        INNER JOIN
    statement_section AS ss ON ss.statement_section_id = ac.balance_sheet_section_id
WHERE
    ss.statement_section_code = 'EQ'
        AND YEAR(je.entry_date) <= varCalendarYear
        AND je.cancelled = 0
	;
    
SELECT 
    COALESCE(SUM(jeli.credit),0) - COALESCE(SUM(jeli.debit),0) INTO varEquityLastYear
FROM
    journal_entry_line_item AS jeli
        INNER JOIN
    account AS ac ON ac.account_id = jeli.account_id
        INNER JOIN
    journal_entry AS je ON je.journal_entry_id = jeli.journal_entry_id
        INNER JOIN
    statement_section AS ss ON ss.statement_section_id = ac.balance_sheet_section_id
WHERE
    ss.statement_section_code = 'EQ'
        AND YEAR(je.entry_date) <= varCalendarYear - 1
        AND je.cancelled = 0
	;

	-- WE DROP THE TMP, TO INSERT THE VALUES INTO THE TMP FORMATTED
	DROP TABLE IF EXISTS gghedini_tmp;
    
  
     
	-- WE CALCULATE THE GROWTH ON REVENUE vs. LAST YEAR
CREATE TABLE gghedini_tmp AS SELECT 'Current Assets                                   ' AS 'Account',
    FORMAT(varCurrentAssetsThisYear, 1) AS This_Year,
    FORMAT(varCurrentAssetsLastYear, 1) Last_Year,
    FORMAT((varCurrentAssetsThisYear / NULLIF(varCurrentAssetsLastYear,0) - 1) * 100,1) AS '% Change YoY';
    
    INSERT INTO gghedini_tmp
        SELECT 	"Fixed Assets",
			FORMAT(varFixAssetsThisYear, 1), 
			FORMAT(varFixAssetsLastYear, 1), 
            FORMAT((varFixAssetsThisYear / NULLIF(varFixAssetsLastYear,0) - 1) * 100, 1);
            
	 INSERT INTO gghedini_tmp
        SELECT 	"Deferred Assets",
			FORMAT(varDeferredAssetsThisYear, 1), 
			FORMAT(varDeferredAssetsLastYear, 1), 
            FORMAT((varDeferredAssetsThisYear / NULLIF(varDeferredAssetsLastYear,0) - 1) * 100, 1);

 INSERT INTO gghedini_tmp
        SELECT 	"Total Assets",
			FORMAT(varCurrentAssetsThisYear + varFixAssetsThisYear + varDeferredAssetsThisYear, 1), 
			FORMAT(varCurrentAssetsLastYear + varFixAssetsLastYear + varDeferredAssetsLastYear, 1), 
			FORMAT((varCurrentAssetsThisYear / NULLIF(varCurrentAssetsLastYear,0) - 1) * 100,1);

    INSERT INTO gghedini_tmp 
		SELECT "", "", "", "";

	 INSERT INTO gghedini_tmp
        SELECT 	"Current Liabilities",
			FORMAT(varCurrentLiabilitiesThisYear, 1), 
			FORMAT(varCurrentLiabilitiesLastYear, 1), 
            FORMAT((varCurrentLiabilitiesThisYear / NULLIF(varCurrentLiabilitiesLastYear,0) - 1) * 100, 1);
  
     
	INSERT INTO gghedini_tmp
        SELECT 	"Long-term Liabilities",
			FORMAT(varLongtermLiabilitiesThisYear, 1), 
			FORMAT(varLongtermLiabilitiesLastYear, 1), 
            FORMAT((varLongtermLiabilitiesThisYear / NULLIF(varLongtermLiabilitiesLastYear,0) - 1) * 100, 1);
   
	 INSERT INTO gghedini_tmp
        SELECT 	"Deferred Liabilities",
			FORMAT(varDeferredLiabilitiesThisYear, 1), 
			FORMAT(varDeferredLiabilitiesLastYear, 1), 
            FORMAT((varDeferredLiabilitiesThisYear / NULLIF(varDeferredLiabilitiesLastYear,0) - 1) * 100, 1);
 
 INSERT INTO gghedini_tmp
        SELECT 	"Total Liabilities",
			FORMAT(varCurrentLiabilitiesThisYear + varLongtermLiabilitiesThisYear + varDeferredLiabilitiesThisYear, 1), 
			FORMAT(varCurrentLiabilitiesLastYear + varLongtermLiabilitiesLastYear + varDeferredLiabilitiesLastYear, 1), 
			FORMAT((varCurrentLiabilitiesThisYear / NULLIF(varCurrentLiabilitiesLastYear,0) - 1) * 100, 1); 
    INSERT INTO gghedini_tmp 
		SELECT "", "", "", "";

	 INSERT INTO gghedini_tmp
        SELECT 	"Equity",
			FORMAT(varCurrentAssetsThisYear - varCurrentLiabilitiesThisYear, 1), 
			FORMAT(varCurrentAssetsLastYear - varCurrentLiabilitiesLastYear, 1), 
            FORMAT((varCurrentAssetsThisYear - varCurrentLiabilitiesThisYear) / (NULLIF(varCurrentAssetsLastYear - varCurrentLiabilitiesLastYear,0) - 1) * 100, 1);

 INSERT INTO gghedini_tmp 
		SELECT "", "", "", "";

	 INSERT INTO gghedini_tmp
        SELECT 	"Assets = Liabilities + Equity",
			FORMAT(varCurrentAssetsThisYear - varCurrentLiabilitiesThisYear - (varCurrentAssetsThisYear - varCurrentLiabilitiesThisYear), 1), 
			FORMAT(varCurrentAssetsLastYear - varCurrentLiabilitiesLastYear - (varCurrentAssetsLastYear - varCurrentLiabilitiesLastYear), 1),
            0.0;

END $$
DELIMITER ;

SET @varYear := 2020;
CALL gghedini_team1(@varYear, @net_income);

SELECT * FROM gghedini_tmp;

CALL gghedini_balance_sheet(@varYear);

SELECT * FROM gghedini_tmp;
