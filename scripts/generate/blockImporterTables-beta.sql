ALTER TABLE txs ADD COLUMN succeeded boolean DEFAULT true;
-- FIXME: Make best block nullable