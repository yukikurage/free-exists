export const unsafeEmptyRecordF = new Map();

export const unsafeConsRecordF = (label) => (value) => (map) => (rf) => {
  const res = new Map(rf);

  res.set(label, {
    value,
    map,
  });

  return res;
};

export const unsafeUnRecord = (rf) => {
  const res = {};

  rf.forEach(({ value }, label) => {
    res[label] = value;
  });

  return res;
};

export const mapImpl = (f) => (rf) => {
  const res = new Map();

  rf.forEach(({ value, map }, label) => {
    res.set(label, { value: map(f)(value), map });
  });

  return res;
};

export const getFImpl = (label) => (rf) => {
  return rf.get(label).value;
};

export const setFImpl = (label) => (value) => (map) => (rf) => {
  const res = new Map(rf);

  res.set(label, {
    value,
    map,
  });

  return res;
};

export const modifyFImpl = (label) => (f) => (map) => (rf) => {
  const res = new Map(rf);

  res.set(label, {
    value: f(rf.get(label).value),
    map,
  });

  return res;
}

export const insertFImpl = (label) => (value) => (map) => (rf) => {
  const res = new Map(rf);

  res.set(label, {
    value,
    map,
  });

  return res;
}

export const deleteFImpl = (label) => (rf) => {
  const res = new Map(rf);

  res.delete(label);

  return res;
}

export const mergeFImpl = (rf1) => (rf2) => {
  const res = new Map(rf2);

  rf1.forEach(({ value, map }, label) => {
    res.set(label, { value, map });
  });

  return res;
}
