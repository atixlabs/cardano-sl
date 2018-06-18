const fs = require('fs');
const ExplorerApi = require('ExplorerApi');

const RECEIVE_TX = "Received tx";
const DECODED_TX = "Decoded tx";
const VERIFY_TX  = "Verified tx";
const PROCESS_TX = "Processed tx";
const SENT_TX    = "Sent tx";

const HASH_IDX    = 0;
const RECEIVE_IDX = 1;
const DECODED_IDX = 2;
const VERIFY_IDX  = 3;
const PROCESS_IDX = 4;
const SENT_IDX    = 5;

const CSV_COLUMN_SEPARATOR = ';';

const CSV_COLUMN_NAMES = [
  "Tx hash",
  "Decoding time",
  "Verification time",
  "Process time",
  "Sending time",
  "Total processing time",
  "Confirmation time",
];

/**
 * On launch processes a log file and produces a csv file with the time taken for each
 * stage of tx processing.
 * Usage:
 *    node processBenchLogs.js INPUT_LOG_FILE OUTPUT_CSV_FILE
 * 
 * The produced file will have the following format:
 * - Tx hash: hash of the tx
 * - Decoding time: Time from receiving the tx and decoding it
 * - Verification time: Time from decoding the tx and verifying it
 * - Process time: Time from verifying the tx and processing it
 * - Sending time: Time from processing the tx and finishing sending it
 * - Total processing time: Time from receiving the tx and finishing sending it
 * - Confirmation time: Time from starting sending the tx and it being included in a block
 */
processBenchLogs(process.argv[2], process.argv[3]);

/**
 * Processes a log file and produces a csv file with the time taken for each stage of
 * tx processing.
 * Any error encountered on any log of a tx will make it not be processed, and
 * any tx with a log missing won't be included in the csv file.
 * 
 * @param {*} inputFile with the logs to be processed
 * @param {*} outputFile, the output csv file
 * @requires all the txs should be confirmed
 */
function processBenchLogs(inputFile, outputFile) {
  fs.readFile(inputFile, 'utf8', function (err, data) {
    if(err) {
      console.log(err);
    } else {
      processBenchLogsData(data, outputFile);
    }
  })

  function processBenchLogsData(data, outputFile) {
    console.log("Started processing logs and generating the data map");

    const processMap = new Map();
    data.split('\n').forEach((line) => {
      try{
        processSingleLog(line, processMap);
      } catch (err) {
        console.log(`WARN: Error processing log ${line} due to ${err}`);
      }
    });
  
    let csvFileStream = fs.createWriteStream(outputFile);
    csvFileStream.write(_getCsvLn(CSV_COLUMN_NAMES));

    console.log("Getting the confirmation times");
    const txHashes = getTxHashes(processMap);
    const promise_con_dates = Promise.all(txHashes.map(([id, hash]) =>
      ExplorerApi.txs.getTimestamp(hash)
        .then(res => Promise.resolve([id, res]))
      ));

    console.log("Processing the data map entries");
    promise_con_dates.then((con_dates) => {
      const con_dates_map = new Map(con_dates);

      processMap.forEach((txData, id) => {
        const con_d = con_dates_map.get(id);
        try {
          const timesLn = processMapEntry(id, txData, con_d);
          const lineToWrite = _getCsvLn(timesLn);
          csvFileStream.write(lineToWrite);
        } catch (err) {
          console.log(`WARN: Error processing entry ${txData} with confirmation time ${con_d} due to ${err}`);
        }
      });
      csvFileStream.end();
      console.log("Finished processing log file");
    });
  }
}


function processMapEntry(id, mapEntry, con_d) {
  const hash = mapEntry[HASH_IDX];
  const rcv_d = new Date(mapEntry[RECEIVE_IDX]);
  const dec_d = new Date(mapEntry[DECODED_IDX]);
  const ver_d = new Date(mapEntry[VERIFY_IDX]);
  const prc_d = new Date(mapEntry[PROCESS_IDX]);
  const snt_d = new Date(mapEntry[SENT_IDX]);

  if(isNaN(rcv_d) || isNaN(dec_d) || isNaN(ver_d) || isNaN(prc_d) || isNaN(snt_d) || isNaN(con_d)){
    throw `Invalid date on entry with id ${id}`;
  }

  return [
      hash,
      dec_d.getTime() - rcv_d.getTime(), // Decoding time
      ver_d.getTime() - dec_d.getTime(), // Verification time
      prc_d.getTime() - ver_d.getTime(), // Process time
      snt_d.getTime() - prc_d.getTime(), // Sending time
      snt_d.getTime() - rcv_d.getTime(), // Total processing time
      con_d.getTime() - prc_d.getTime(), // Confirmation time (time from starting to send
                                         //                         to incorporation on block)
  ];
}


function processSingleLog(logLn, processMap) {
  const [date, id, action] = /^\[.+?\] \[(.+?)\] \[(.+?)\] (.+?)$/.exec(logLn).slice(1, 4);
  
  const txTimes = _getOrEmpty(processMap, id);

  const idx = actionToIndex(action);
  if(idx == HASH_IDX){
    txTimes[idx] = /^Has tx hash (.+?)$/.exec(action)[1];
  } else {
    txTimes[idx] = date;
  }
}

function actionToIndex(action) {
  switch(action) {
    case RECEIVE_TX:
      return RECEIVE_IDX;
    case DECODED_TX:
      return DECODED_IDX;
    case VERIFY_TX:
      return VERIFY_IDX;
    case PROCESS_TX:
      return PROCESS_IDX;
    case SENT_TX:
      return SENT_IDX;
    default: // If the action doesn't match other then it's the hash equality
      return HASH_IDX;
  }
}

function getTxHashes(processMap) {
  const txHashes = [];
  processMap.forEach((txData, id) => {
    const hash = txData[HASH_IDX]
    if(!!hash) {
      txHashes.push([id, hash]);
    }
  });
  return txHashes;
}


function _getOrEmpty(map, key) {
  let currentValue = map.get(key);
  if(!currentValue) {
    map.set(key, []);
    currentValue = map.get(key);
  }
  return currentValue;
}

function _getCsvLn(line) {
  return line.join(CSV_COLUMN_SEPARATOR) + '\n'; 
}
